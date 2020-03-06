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
import "ghc" Outputable ( Outputable, showSDoc, ppr )
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
    uniqSupply <- mkSplitUniqSupply 'z'
    return ( initNameCache uniqSupply [] )

  dynFlags <- do
    systemSettings <- initSysTools libdir
    return ( defaultDynFlags systemSettings ( [], [] ) )

  moreRoots <-
    liftIO do
      ( HieFileResult{ hie_file_result }, _ ) <-
        readHieFile nameCache "./dist-newstyle/build/x86_64-linux/ghc-8.8.1/circuithub-api-0.0.4/noopt/build/Handler.hie"

      return $
        foldMap
          ( \case
              Avail name -> foldMap Set.singleton ( nameToDeclaration name )
              _ -> mempty
          )
          ( hie_exports hie_file_result )

  analysis <-
    flip execStateT emptyAnalysis do
      for_ hieFilePaths \hieFilePath -> do
        liftIO ( putStrLn ( "Processing " ++ hieFilePath ) )

        ( HieFileResult{ hie_file_result }, _ ) <-
          liftIO ( readHieFile nameCache hieFilePath )

        analyseHieFile keepExports hie_file_result


  let
    reachableSet =
      reachable analysis ( moreRoots <> roots )

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

    warnings =
      Map.unionsWith (++) $
      foldMap
        ( \d ->
            [ Map.unionsWith (++) $
              foldMap
                ( \_ ->
                    [ Map.singleton ( declModule d ) [ d ] ]
                )
                ( Map.lookup d ( declarationSites analysis ) )
            ]
        )
        dead

  traverse_ ( putStrLn . showSDoc dynFlags . ppr . moduleUnitId ) ( Map.keys warnings )

  for_ ( Map.toList warnings ) \( m, declarations ) -> do
    putStrLn $ moduleNameString $ moduleName m
    for_ declarations \d ->
      putStrLn $ "  - " <> occNameString ( declOccName d )
    putStrLn ""
