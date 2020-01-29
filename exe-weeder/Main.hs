{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main ( main ) where

import "algebraic-graphs" Algebra.Graph ( Graph, edge, empty, overlay, overlays, vertex, vertexList )
import "algebraic-graphs" Algebra.Graph.Export.Dot ( defaultStyle, export, vertexAttributes, Attribute( (:=) ) )
import "algebraic-graphs" Algebra.Graph.ToGraph ( dfs, dfsForest )

import "ansi-terminal" System.Console.ANSI
  ( Color( Red, White )
  , ColorIntensity( Vivid )
  , ConsoleLayer( Foreground, Background )
  , SGR( SetColor )
  , setSGRCode
  )

import "base" Control.Applicative ( (<**>), Alternative, many, some )
import "base" Control.Monad ( guard, mfilter, msum, unless )
import "base" Control.Monad.IO.Class ( liftIO )
import "base" Data.Foldable ( for_, traverse_, toList )
import "base" Data.List ( intercalate )
import "base" Data.Maybe ( maybeToList )
import "base" Data.Monoid ( First( First ) )
import "base" Debug.Trace
import "base" System.Environment ( getArgs )

import "bytestring" Data.ByteString.Char8 ( unpack )

import "containers" Data.Map.Strict ( Map )
import qualified "containers" Data.Map.Strict as Map
import "containers" Data.Sequence ( Seq )
import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as Set
import "containers" Data.Tree ( rootLabel, subForest )

import "directory" System.Directory ( doesPathExist, withCurrentDirectory, canonicalizePath, listDirectory, doesFileExist, doesDirectoryExist )

import "extra" Data.List.Extra ( split )

import "filepath" System.FilePath ( isExtensionOf )

import "ghc" Avail ( AvailInfo( Avail, AvailTC ) )
import "ghc" DynFlags ( DynFlags, defaultDynFlags )
import "ghc" FastString ( unpackFS )
import "ghc" HieBin ( HieFileResult( HieFileResult, hie_file_result ) )
import "ghc" HieBin ( readHieFile )
import "ghc" HieTypes
  ( BindType( RegularBind )
  , DeclType( DataDec, ConDec )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl )
  , HieAST( Node, nodeInfo, nodeChildren, nodeSpan )
  , HieASTs( HieASTs, getAsts )
  , HieFile( HieFile, hie_asts, hie_hs_src, hie_exports, hie_module )
  , IdentifierDetails( IdentifierDetails, identInfo )
  , NodeInfo( NodeInfo, nodeIdentifiers, nodeAnnotations )
  , Scope( ModuleScope )
  )
import "ghc" Module
  ( DefUnitId( DefUnitId )
  , Module( Module )
  , UnitId( DefiniteUnitId )
  , mkModuleName
  , moduleName
  , moduleNameString
  , moduleStableString
  , moduleUnitId
  , stringToInstalledUnitId
  , unitIdFS
  )
import "ghc" Name ( Name, nameOccName, nameModule_maybe )
import "ghc" NameCache ( initNameCache )
import "ghc" OccName
  ( OccName
  , isDataOcc
  , isDataSymOcc
  , isTcOcc
  , isTvOcc
  , isVarOcc
  , mkOccName
  , occNameString
  , varName
  )
import "ghc" Outputable ( Outputable, showSDoc )
import "ghc" SrcLoc ( RealSrcSpan, srcLocLine, srcLocCol, realSrcSpanStart, realSrcSpanEnd )
import "ghc" SysTools ( initSysTools )
import "ghc" UniqSupply ( mkSplitUniqSupply )

import "ghc-paths" GHC.Paths ( libdir )

import "mtl" Control.Monad.Reader.Class ( MonadReader, ask )
import "mtl" Control.Monad.State.Class ( MonadState, modify' )

import "optparse-applicative" Options.Applicative
  ( Parser
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , maybeReader
  , metavar
  , option
  , showDefault
  , strArgument
  , strOption
  , switch
  , value
  )

import "transformers" Control.Monad.Trans.Maybe ( MaybeT, runMaybeT )
import "transformers" Control.Monad.Trans.Reader ( runReaderT )
import "transformers" Control.Monad.Trans.State.Strict ( execStateT )

import Weeder


data CommandLineArguments =
  CommandLineArguments
    { hiePaths :: [ FilePath ]
    , keepExports :: Bool
    , roots :: Set Declaration
    , units :: Set String
    }


commandLineArgumentsParser :: Parser CommandLineArguments
commandLineArgumentsParser = do
  hiePaths <-
    some
      ( strArgument
          (  metavar "HIE"
          <> help "A path to a .hie file, or a directory containing .hie files"
          )
      )

  keepExports <-
    switch
      (  long "keep-exports"
      <> help "Add all exported symbols to the root set"
      )

  roots <-
    many
      ( option
          ( maybeReader \str -> do
              unit : sym <-
                Just ( split (=='.') str )

              sym : revMod <-
                Just ( reverse sym )

              return
                Declaration
                  { declModule =
                      Module
                        ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId unit ) ) )
                        ( mkModuleName ( intercalate "." ( reverse revMod ) ) )
                  , declOccName =
                      mkOccName varName sym
                  }
          )
          (  long "root"
          <> help "A symbol that should be added to the root set. Symbols are of the form unit.Module.Name.symbol"
          )
      )

  units <-
    many
      ( strOption
          (  long "report-unit"
          <> help "Report unused declarations in this unit. If ommitted, all units will be reported. Can be supplied multiple times."
          )
      )

  return
    CommandLineArguments
      { hiePaths
      , keepExports
      , roots = Set.fromList roots
      , units = Set.fromList units
      }


main :: IO ()
main = do
  CommandLineArguments{ hiePaths, roots, units, keepExports } <-
    execParser
      ( info
          ( commandLineArgumentsParser <**> helper )
          (  fullDesc
          <> header "Find unused declarations in Haskell projects"
          )
      )

  hieFilePaths <-
    foldMap getHieFilesIn hiePaths

  nameCache <- do
    uniqSupply <-
      mkSplitUniqSupply 'z'

    return ( initNameCache uniqSupply [] )

  dynFlags <- do
    systemSettings <-
      initSysTools libdir

    return ( defaultDynFlags systemSettings ( [], [] ) )

  analysis <-
    flip execStateT emptyAnalysis do
      for_ hieFilePaths \hieFilePath -> do
        liftIO ( putStrLn ( "Processing " ++ hieFilePath ) )

        ( HieFileResult{ hie_file_result }, _ ) <-
          liftIO ( readHieFile nameCache hieFilePath )

        analyseHieFile keepExports hie_file_result

        -- liftIO ( print ( Map.keys ( getAsts ( hie_asts hie_file_result ))))

        -- liftIO ( putStrLn ( foldMap ( showSDoc dynFlags . ppHie ) ( getAsts ( hie_asts hie_file_result ) ) ) )

  let
    reachableSet =
      reachable analysis roots

    dead =
      Set.filter
        ( \d ->
            if Set.null units then
              True

            else
              Set.member
                ( unpackFS ( unitIdFS ( moduleUnitId ( declModule d ) ) ) )
                units
        )
        ( allDeclarations analysis Set.\\ reachableSet )

    highlightingMap =
      Map.unionsWith
        overlayHighlight
        ( foldMap
            ( \d ->
                foldMap
                  ( foldMap \m -> [ highlight m ] )
                  ( Map.lookup d ( declarationSites analysis ) )
            )
            dead
        )

  writeFile
    "graph.dot"
    ( export
        ( defaultStyle declarationStableName )
        { vertexAttributes = \v -> [ "label" := occNameString ( declOccName v ) ] }
        ( dependencyGraph analysis )
    )

  let
    go [] _ =
      return ()
    go ( ( module_, moduleSource ) : modules ) dead =
      let
        ( deadHere, deadElsewhere ) =
          Set.partition
            ( \Declaration{ declModule } -> declModule == module_ )
            dead

        defined =
          Set.filter
            ( `Map.member` ( declarationSites analysis ) )
            deadHere

        highlightingMap =
          Map.unionsWith
            overlayHighlight
            ( foldMap
                ( \d ->
                    foldMap
                      ( foldMap ( \m -> [ highlight m ] ) )
                      ( Map.lookup d ( declarationSites analysis ) )
                )
                defined
            )

      in do
      unless ( Set.null deadHere ) do
        putStrLn
          (    "Found "
            ++ show ( Set.size defined )
            ++ " unused declarations in "
            ++ moduleNameString ( moduleName module_ )
            ++ ":"
          )

        putStrLn ""

        putStrLn
          ( unlines
              ( foldMap
                  ( pure . ( "  - " ++ ) . declarationStableName )
                  deadHere
              )
          )

        putStrLn
          ( zipHighlighting
              ( Map.toList highlightingMap )
              ( zip [ 1 .. ] ( lines moduleSource ) )
          )

      go modules deadElsewhere

  go ( Map.toList ( moduleSource analysis ) ) dead


data Skip =
  Skip Int Highlight | SkipToEndOfLine
  deriving ( Show )


data Highlight =
  Highlight Int Skip | HighlightToEndOfLine
  deriving ( Show )


overlayHighlight :: Highlight -> Highlight -> Highlight
overlayHighlight HighlightToEndOfLine _ =
  HighlightToEndOfLine
overlayHighlight _ HighlightToEndOfLine =
  HighlightToEndOfLine
overlayHighlight ( Highlight x xs ) ( Highlight y ys ) =
  case compare x y of
    LT ->
      case dropSkip ( y - x ) xs of
        Left skip ->
          Highlight y ( overlaySkip skip ys )

        Right HighlightToEndOfLine ->
          HighlightToEndOfLine

        Right highlight ->
          overlaySkipHighlight ys highlight

    EQ ->
      Highlight x ( overlaySkip xs ys )

    GT ->
      overlayHighlight ( Highlight y ys ) ( Highlight x xs )



overlaySkip :: Skip -> Skip -> Skip
overlaySkip SkipToEndOfLine x =
  x
overlaySkip x SkipToEndOfLine =
  x
overlaySkip ( Skip x xs ) ( Skip y ys ) =
  case compare x y of
    LT ->
      Skip x ( overlaySkipHighlight ( Skip ( y - x ) ys ) xs )

    EQ ->
      Skip x ( overlayHighlight xs ys )

    GT ->
      Skip y ( overlaySkipHighlight ( Skip ( x - y ) xs ) ys )


overlaySkipHighlight :: Skip -> Highlight -> Highlight
overlaySkipHighlight _ HighlightToEndOfLine =
  HighlightToEndOfLine
overlaySkipHighlight SkipToEndOfLine h =
  h
overlaySkipHighlight ( Skip x xs ) ( Highlight y ys ) =
  case compare x y of
    LT ->
      case dropHighlight ( y - x ) xs of
        Left skip ->
          Highlight y ( overlaySkip skip ys )

        Right highlight ->
          case overlaySkipHighlight ys xs of
            HighlightToEndOfLine ->
              HighlightToEndOfLine

            Highlight z zs ->
              Highlight ( y + z ) zs

    EQ ->
      case overlaySkipHighlight ys xs of
        HighlightToEndOfLine ->
          HighlightToEndOfLine

        Highlight z zs ->
          Highlight ( y + z ) zs

    GT ->
      Highlight y ( overlaySkip ( Skip ( x - y ) xs ) ys )


dropSkip :: Int -> Skip -> Either Skip Highlight
dropSkip _ SkipToEndOfLine =
  Left SkipToEndOfLine
dropSkip x ( Skip y highlight ) =
  case compare x y of
    LT ->
      Left ( Skip ( y - x ) highlight )

    EQ ->
      Right highlight

    GT ->
      dropHighlight ( x - y ) highlight


dropHighlight :: Int -> Highlight -> Either Skip Highlight
dropHighlight _ HighlightToEndOfLine =
  Right HighlightToEndOfLine
dropHighlight x ( Highlight y skip ) =
  case compare x y of
    LT ->
      Right ( Highlight ( y - x ) skip )

    EQ ->
      Left skip

    GT ->
      dropSkip ( x - y ) skip


highlight :: RealSrcSpan -> Map Int Highlight
highlight span =
  if startLine == endLine then
    Map.singleton
      startLine
      ( Highlight
          0
          ( Skip
              ( startCol - 1 )
              ( Highlight
                  ( endCol - startCol )
                  SkipToEndOfLine
              )
          )
      )

  else
    Map.fromList
      ( concat
          [ pure ( startLine, Highlight 0 ( Skip ( startCol - 1 ) HighlightToEndOfLine ) )
          , [ ( l, HighlightToEndOfLine ) | l <- [ startLine + 1 .. endLine - 1 ] ]
          , pure ( endLine, Highlight ( endCol - 1 ) SkipToEndOfLine )
          ]
      )

  where

    startCol =
      srcLocCol start

    startLine =
      srcLocLine start

    endCol =
      srcLocCol end

    endLine =
      srcLocLine end

    start =
      realSrcSpanStart span

    end =
      realSrcSpanEnd span


zipHighlighting
  :: [ ( Int, Highlight ) ]
  -> [ ( Int, String ) ]
  -> String
zipHighlighting =
  highlightWithContext 3 1

  where

    highlightWithContext
      :: Int -> Int -> [ ( Int, Highlight ) ] -> [ ( Int, String ) ] -> String
    highlightWithContext _ currLine ( ( i, highlight ) : hs ) [] =
      ""
    highlightWithContext _ currLine [] _ =
      ""
    highlightWithContext n currLine ( ( i, highlight ) : hs ) ( ( linum, l ) : ls ) =
      case compare currLine i of
        LT | i - currLine > n ->
          highlightWithContext
            n
            ( currLine + 1 )
            ( ( i, highlight ) : hs )
            ls

        LT ->
             "    "
          ++ show linum
          ++ " │ "
          ++  l
          ++ "\n"
          ++ highlightWithContext
               ( n - 1 )
               ( currLine + 1 )
               ( ( i, highlight ) : hs )
               ls

        EQ ->
             "    "
          ++ show linum
          ++ " │ "
          ++ highlightString highlight l
          ++ "\n"
          ++ trailingContext 3 ( currLine + 1 ) hs ls

        GT ->
          error "Forgot to highlight something!"

    trailingContext
      :: Int -> Int -> [ ( Int, Highlight ) ] -> [ ( Int, String ) ] -> String
    trailingContext n currLine _ [] =
      ""
    trailingContext n currLine [] ( ( linum, l ) : ls ) =
      if n > 0 then
           "    "
        ++ show linum
        ++ " │ "
        ++ l
        ++ "\n"
        ++ trailingContext ( n - 1 ) ( currLine + 1 ) [] ls
      else
        ""
    trailingContext n currLine ( ( i, highlight ) : hs ) ( ( linum, l ) : ls ) =
      case compare currLine i of
        LT | n > 0 ->
             "    "
          ++ show linum
          ++ " │ "
          ++ l
          ++ "\n"
          ++ trailingContext ( n - 1 ) ( currLine + 1 ) ( ( i, highlight ) : hs ) ls

        LT ->
             "\n"
          ++ highlightWithContext 3 ( currLine + 1 ) ( ( i, highlight ) : hs ) ls

        EQ ->
             "    "
          ++ show linum
          ++ " │ "
          ++ highlightString highlight l
          ++ "\n"
          ++ trailingContext 3 ( currLine + 1 ) hs ls

        GT ->
          error "Forgot to highlight!"


highlightString :: Highlight -> String -> String
highlightString HighlightToEndOfLine s =
  hlCode
    <> s
    <> setSGRCode []
highlightString ( Highlight n skip ) s =
  hlCode
    <> take n s
    <> setSGRCode []
    <> skipThenHighlight skip ( drop n s )


skipThenHighlight :: Skip -> String -> String
skipThenHighlight SkipToEndOfLine s =
  s
skipThenHighlight ( Skip n h ) s =
  take n s <> highlightString h ( drop n s )


hlCode =
  setSGRCode [ SetColor Background Vivid Red, SetColor Foreground Vivid White ]


-- | Recursively search for .hie files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
getHieFilesIn path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <-
        doesFileExist path

      if isFile && "hie" `isExtensionOf` path
        then do
          path' <-
            canonicalizePath path

          return [ path' ]

        else do
          isDir <-
            doesDirectoryExist path

          if isDir
            then do
              cnts <-
                listDirectory path

              withCurrentDirectory path ( foldMap getHieFilesIn cnts )

            else
              return []

    else
      return []
