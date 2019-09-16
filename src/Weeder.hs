{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Weeder
  ( Declaration( Declaration, declModule, declOccName )
  , allDeclarations
  , analyseHieFile
  , declarationSites
  , declarationStableName
  , dependencyGraph
  , emptyAnalysis
  , implicitRoots
  , moduleSource
  , reachable
  )
  where

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

import "filepath" System.FilePath ( isExtensionOf )

import "ghc" Avail ( AvailInfo( Avail, AvailTC ) )
import "ghc" DynFlags ( DynFlags, defaultDynFlags )
import "ghc" HieBin ( HieFileResult( HieFileResult, hie_file_result ) )
import "ghc" HieBin ( readHieFile )
import "ghc" HieDebug ( ppHie )
import "ghc" HieTypes
  ( BindType( RegularBind )
  , DeclType( DataDec, ClassDec, ConDec )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl, ClassTyDecl )
  , HieAST( Node, nodeInfo, nodeChildren, nodeSpan )
  , HieASTs( HieASTs )
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
  , moduleNameString
  , moduleName
  , moduleStableString
  , stringToInstalledUnitId
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
  , switch
  , value
  )

import "transformers" Control.Monad.Trans.Maybe ( MaybeT, runMaybeT )
import "transformers" Control.Monad.Trans.Reader ( runReaderT )
import "transformers" Control.Monad.Trans.State.Strict ( execStateT )


data CommandLineArguments =
  CommandLineArguments
    { hiePaths :: [ FilePath ]
    , keepExports :: Bool
    , roots :: Set Declaration
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
                Just ( words str )

              sym : revMod <-
                Just ( reverse sym )

              return
                Declaration
                  { declModule =
                      Module
                        ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId unit ) ) )
                        ( mkModuleName ( unwords ( reverse revMod ) ) )
                  , declOccName =
                      mkOccName varName sym
                  }
          )
          (  long "root"
          <> help "A symbol that should be added to the root set. Symbols are of the form unit$Module.symbol"
          )
      )


  return
    CommandLineArguments
      { hiePaths
      , keepExports
      , roots = Set.fromList roots
      }


main :: IO ()
main = do
  CommandLineArguments{ hiePaths, roots } <-
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

        analyseHieFile hie_file_result


        -- liftIO ( putStrLn ( foldMap ( showSDoc dynFlags . ppHie ) hieASTs ) )

  let
    reachableSet =
      reachable analysis roots

    dead =
      allDeclarations analysis Set.\\ reachableSet

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


data Declaration =
  Declaration
    { declModule :: Module
    , declOccName :: OccName
    }
  deriving
    ( Eq, Ord )


instance Show Declaration where
  show =
    declarationStableName


declarationStableName :: Declaration -> String
declarationStableName Declaration { declModule, declOccName } =
  let
    namespace =
      if isVarOcc declOccName then
        "var"

      else if isTvOcc declOccName then
        "tv"

      else if isTcOcc declOccName then
        "tc"

      else if isDataOcc declOccName then
        "data"

      else if isDataSymOcc declOccName then
        "dataSym"

      else
        "unknown"

    in
    intercalate "$" [ namespace, moduleStableString declModule, "$", occNameString declOccName ]


data Analysis =
  Analysis
    { dependencyGraph :: Graph Declaration
      -- ^ A graph between declarations, capturing dependencies.
    , declarationSites :: Map Declaration ( Set RealSrcSpan )
      -- ^ A partial mapping between declarations and their definition site.
      -- This Map is partial as we don't always know where a Declaration was
      -- defined (e.g., it may come from a package without source code).
      -- We capture a set of spans, because a declaration may be defined in
      -- multiple locations, e.g., a type signature for a function separate
      -- from its definition.
    , implicitRoots :: Set Declaration
      -- ^ The Set of all Declarations that are always reachable. This is used
      -- to capture knowledge not yet modelled in weeder, such as instance
      -- declarations depending on top-level functions.
    , moduleSource :: Map Module String
      -- ^ Map Modules back to their source code.
    }


emptyAnalysis :: Analysis
emptyAnalysis =
  Analysis empty mempty mempty mempty


reachable :: Analysis -> Set Declaration -> Set Declaration
reachable Analysis{ dependencyGraph, implicitRoots } roots =
  Set.fromList
    ( dfs ( Set.toList ( Set.union implicitRoots roots ) ) dependencyGraph )


allDeclarations :: Analysis -> Set Declaration
allDeclarations Analysis{ dependencyGraph } =
  Set.fromList ( vertexList dependencyGraph )


analyseHieFile :: MonadState Analysis m => HieFile -> m ()
analyseHieFile HieFile{ hie_hs_src, hie_asts = HieASTs hieASTs, hie_exports, hie_module } = do
  modify' \a ->
    a
      { moduleSource =
          Map.insert hie_module ( unpack hie_hs_src ) ( moduleSource a )
      }

  for_ hieASTs \ast ->
    addAllDeclarations ast >> topLevelAnalysis ast

  for_ hie_exports analyseExport


analyseExport :: MonadState Analysis m => AvailInfo -> m ()
analyseExport = \case
  Avail name ->
    for_ ( nameToDeclaration name ) addImplicitRoot

  AvailTC name pieces _ ->
       for_ ( nameToDeclaration name ) addImplicitRoot
    >> for_ pieces ( traverse_ addImplicitRoot . nameToDeclaration )


-- | @addDependency x y@ adds the information that @x@ depends on @y@.
addDependency :: MonadState Analysis m => Declaration -> Declaration -> m ()
addDependency x y =
  modify' \a ->
    a { dependencyGraph = overlay ( dependencyGraph a ) ( edge x y ) }


addImplicitRoot :: MonadState Analysis m => Declaration -> m ()
addImplicitRoot x =
  modify' \a -> a { implicitRoots = implicitRoots a <> Set.singleton x }


define :: MonadState Analysis m => Declaration -> RealSrcSpan -> m ()
define decl span = do
  modify' \a ->
    a
      { declarationSites =
          Map.unionWith
            Set.union
            ( declarationSites a )
            ( Map.singleton decl ( Set.singleton span ) )
      , dependencyGraph =
          overlay ( dependencyGraph a ) ( vertex decl )
      }


addDeclaration :: ( MonadState Analysis m ) => Declaration -> m ()
addDeclaration decl = do
  modify' \a ->
    a
      { dependencyGraph =
          overlay ( dependencyGraph a ) ( vertex decl )
      }


-- | Try and add vertices for all declarations in an AST - both
-- those declared here, and those referred to from here.
addAllDeclarations :: ( MonadState Analysis m ) => HieAST a -> m ()
addAllDeclarations n@Node{ nodeChildren } = do
  for_ ( findIdentifiers ( const True ) n ) addDeclaration

  for_ nodeChildren addAllDeclarations


topLevelAnalysis :: MonadState Analysis m => HieAST a -> m ()
topLevelAnalysis n@Node{ nodeChildren } = do
  analysed <-
    runMaybeT
      ( msum
          [
          --   analyseStandaloneDeriving n
          -- ,
            analyseBinding n
          -- , analyseRewriteRule n
          , analyseInstanceDeclaration n
          , analyseClassDeclaration n
          -- , analyseDataDeclaration n
          ]
      )

  case analysed of
    Nothing ->
      -- We didn't find a top level declaration here, check all this nodes
      -- children.
      traverse_ topLevelAnalysis nodeChildren

    Just () ->
      -- Top level analysis succeeded, there's nothing more to do for this node.
      return ()


-- | Analyse standalone deriving declarations
analyseStandaloneDeriving :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseStandaloneDeriving n@Node{ nodeInfo = NodeInfo{ nodeAnnotations }, nodeChildren } = do
  guard ( ( "DerivDecl", "DerivDecl" ) `Set.member` nodeAnnotations )

  for_ ( uses n ) addImplicitRoot


-- | Try and analyse binding-like nodes. This includes function bindings,
-- type signatures, pattern bindings and type synonyms.
analyseBinding :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseBinding n@Node{ nodeChildren, nodeSpan, nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard
   ( or
       [ ( "TypeSig", "Sig" ) `Set.member` nodeAnnotations
       , ( "FunBind", "HsBindLR" ) `Set.member` nodeAnnotations
       -- , ( "PatBind", "HsBindLR" ) `Set.member` nodeAnnotations
       -- , ( "SynDecl", "TyClDecl" ) `Set.member` nodeAnnotations
       ]
   )

  for_ ( findDeclarations n ) \d -> do
    define d nodeSpan

    for_ ( uses n ) \use ->
      addDependency d use


analyseRewriteRule :: ( Alternative m, MonadState Analysis m, MonadReader String m ) => HieAST a -> m ()
analyseRewriteRule n@Node{ nodeChildren, nodeSpan, nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "HsRule", "RuleDecl" ) `Set.member` nodeAnnotations )

  for_ ( uses n ) addImplicitRoot


analyseInstanceDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseInstanceDeclaration n@Node{ nodeChildren, nodeSpan, nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "ClsInstD", "InstDecl" ) `Set.member` nodeAnnotations )

  traverse_ addImplicitRoot ( uses n )


analyseClassDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseClassDeclaration n@Node{ nodeChildren, nodeSpan, nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "ClassDecl", "TyClDecl" ) `Set.member` nodeAnnotations )

  for_ ( findIdentifiers isClassDeclaration n ) \d -> do
    addImplicitRoot d

    for_ ( findIdentifiers ( const True ) n ) ( addDependency d )

  where

    isClassDeclaration =
      not . Set.null . Set.filter \case
        Decl ClassDec _ ->
          True

        _ ->
          False


analyseDataDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader String m ) => HieAST a -> m ()
analyseDataDeclaration n@Node { nodeSpan, nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "DataDecl", "TyClDecl" ) `Set.member` nodeAnnotations )

  for_
    ( foldMap
        ( First . Just )
        ( findIdentifiers ( any isDataDec ) n )
    )
    \dataTypeName -> do
      define dataTypeName nodeSpan

      for_ ( constructors n ) \constructor ->
        for_ ( foldMap ( First . Just ) ( findIdentifiers ( any isConDec ) constructor ) ) \conDec -> do
          addDependency conDec dataTypeName

          for_ ( uses constructor ) ( addDependency conDec )

  where

    isDataDec ( Decl DataDec _ ) =
      True
    isDataDec _ =
      False

    isConDec ( Decl ConDec _ ) =
      True
    isConDec _ =
      False


constructors :: HieAST a -> Seq ( HieAST a )
constructors n@Node { nodeChildren, nodeInfo = NodeInfo{ nodeAnnotations } } =
  if any ( \( _, t ) -> t == "ConDecl" ) nodeAnnotations then
    pure n

  else
    foldMap constructors nodeChildren


findDeclarations :: HieAST a -> Seq Declaration
findDeclarations =
  findIdentifiers
    (   not
      . Set.null
      . Set.filter \case
          ValBind RegularBind ModuleScope _ ->
            True

          PatternBind ModuleScope _ _ ->
            True

          Decl _ _ ->
            True

          TyDecl ->
            True

          ClassTyDecl{} ->
            True

          _ ->
            False
    )


findIdentifiers
  :: ( Set ContextInfo -> Bool )
  -> HieAST a
  -> Seq Declaration
findIdentifiers f Node{ nodeInfo = NodeInfo{ nodeIdentifiers }, nodeChildren } =
     foldMap
       ( \case
           ( Left _, _ ) ->
             mempty

           ( Right name, IdentifierDetails{ identInfo } ) ->
             if f identInfo then
               foldMap pure ( nameToDeclaration name )

             else
               mempty
           )

       ( Map.toList nodeIdentifiers )
  <> foldMap ( findIdentifiers f ) nodeChildren


uses :: HieAST a -> Set Declaration
uses =
    foldMap Set.singleton
  . findIdentifiers \identInfo -> Use `Set.member` identInfo



nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <-
    nameModule_maybe name

  return Declaration { declModule = m, declOccName = nameOccName name }


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
