{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main ( main ) where

import "base" Control.Applicative ( (<**>), liftA2, many, some )
import "base" Control.Monad ( guard )
import "base" Control.Monad.IO.Class ( liftIO )
import "base" Data.Foldable ( fold, for_ )
import "base" Data.List ( intercalate )

import qualified "containers" Data.Map.Strict as Map
import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as Set

import "directory" System.Directory ( doesPathExist, withCurrentDirectory, canonicalizePath, listDirectory, doesFileExist, doesDirectoryExist )

import "filepath" System.FilePath ( isExtensionOf )

import "ghc" FastString ( unpackFS )
import "ghc" HieBin ( HieFileResult( HieFileResult, hie_file_result ) )
import "ghc" HieBin ( readHieFile )
import "ghc" Module
  ( DefUnitId( DefUnitId )
  , Module( Module )
  , UnitId( DefiniteUnitId )
  , mkModuleName
  , moduleUnitId
  , stringToInstalledUnitId
  , unitIdFS
  )
import "ghc" NameCache ( initNameCache )
import "ghc" OccName
  ( mkOccName
  , occNameString
  , varName
  )
import "ghc" SrcLoc ( srcLocLine, srcLocCol, realSrcSpanStart )
import "ghc" UniqSupply ( mkSplitUniqSupply )

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
  , strArgument
  , strOption
  )

import "transformers" Control.Monad.Trans.State.Strict ( execStateT )

import Weeder


data CommandLineArguments =
  CommandLineArguments
    { hiePaths :: [ FilePath ]
    , roots :: Set Root
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

  roots <-
    many
      ( option
          ( maybeReader \str ->
              case words str of
                [ unitId, moduleName, sym ] ->
                  return $
                  DeclarationRoot $
                  Declaration
                    { declModule =
                        Module
                          ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId unitId ) ) )
                          ( mkModuleName moduleName )
                    , declOccName =
                        mkOccName varName sym
                    }

                [ unitId, moduleName ] ->
                  return $
                  ModuleRoot $
                  Module
                    ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId unitId ) ) )
                    ( mkModuleName moduleName )

                _ ->
                  Nothing
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
      , roots = Set.fromList roots
      , units = Set.fromList units
      }


main :: IO ()
main = do
  CommandLineArguments{ hiePaths, roots, units } <-
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

  analysis <-
    flip execStateT emptyAnalysis do
      for_ hieFilePaths \hieFilePath -> do
        ( HieFileResult{ hie_file_result }, _ ) <-
          liftIO ( readHieFile nameCache hieFilePath )

        analyseHieFile hie_file_result

  let
    reachableSet =
      reachable
        analysis
        ( roots <> Set.map DeclarationRoot ( implicitRoots analysis ) )

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
            fold $ do
              moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )

              spans <- Map.lookup d ( declarationSites analysis )
              guard $ not $ null spans

              return [ Map.singleton moduleFilePath ( liftA2 (,) (Set.toList spans) (pure d) ) ]
        )
        dead

  for_ ( Map.toList warnings ) \( path, declarations ) ->
    for_ declarations \( srcSpan, d ) -> do
      let start = realSrcSpanStart srcSpan

      putStrLn $
        unwords
          [ intercalate ":" [ path, show ( srcLocLine start ), show ( srcLocCol start ) ]
          , occNameString ( declOccName d )
          ]


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
