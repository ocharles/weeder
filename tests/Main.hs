{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language PackageImports #-}
{-# language NondecreasingIndentation #-}

module Main where

import "base" Control.Monad ( unless )
import "base" Control.Monad.IO.Class ( liftIO )
import "base" Data.Foldable ( for_, traverse_, toList )

import qualified "containers" Data.Set as Set

import "directory" System.Directory ( doesPathExist, withCurrentDirectory, canonicalizePath, listDirectory, doesFileExist, doesDirectoryExist )

import "filepath" System.FilePath ( isExtensionOf, takeBaseName )

import "ghc" GHC
  ( Target( Target, targetId, targetAllowObjCode, targetContents )
  , LoadHowMuch( LoadAllTargets )
  , TargetId ( TargetFile )
  , TypecheckedModule( TypecheckedModule, tm_internals_, tm_renamed_source )
  , addTarget
  , getModSummary
  , getSession
  , getSessionDynFlags
  , load
  , mkModuleName
  , parseModule
  , runGhc
  , setSessionDynFlags
  , typecheckModule
  )
import "ghc" HieAst ( mkHieFile )
import "ghc" HieTypes ( hie_asts, getAsts )
import "ghc" HieDebug ( ppHie )
import "ghc" HscTypes ( runHsc )
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
import "ghc" OccName ( mkOccName, varName )
import "ghc" Outputable ( showSDoc )

import "ghc-paths" GHC.Paths ( libdir )

import "tasty" Test.Tasty ( TestTree, defaultMain, testGroup )

import "tasty-hunit" Test.Tasty.HUnit ( testCase )

import "transformers" Control.Monad.Trans.State.Strict ( execState )

import "weeder" Weeder
  ( allDeclarations
  , Declaration( Declaration, declModule, declOccName )
  , analyseHieFile
  , declarationStableName
  , emptyAnalysis
  , implicitRoots
  , reachable
  )

main :: IO ()
main = do
  discoverTests >>= defaultMain . testGroup "Tests"


discoverTests :: IO [ TestTree ]
discoverTests =
  fmap ( map fileToTestCase ) ( getTestCasesIn "tests/test-cases" )


fileToTestCase :: FilePath -> TestTree
fileToTestCase sourceFilePath =
    testCase ( takeBaseName sourceFilePath )
  $ runGhc ( Just libdir ) do
      dynFlags <-
        getSessionDynFlags

      setSessionDynFlags dynFlags

      addTarget
        Target
          { targetId = TargetFile sourceFilePath Nothing
          , targetAllowObjCode = False
          , targetContents = Nothing
          }

      load LoadAllTargets

      let
        testModuleName =
          mkModuleName "Test"

      modSummary <-
        getModSummary testModuleName

      parseModule modSummary
        >>= typecheckModule
        >>= \case
              TypecheckedModule{ tm_renamed_source = Just renamedSource, tm_internals_ = ( tcGblEnv, _ ) } -> do
                hscEnv <-
                  getSession

                hieFile <-
                  liftIO ( runHsc hscEnv ( mkHieFile modSummary tcGblEnv renamedSource ) )

                let
                  analysis =
                    execState ( analyseHieFile False hieFile ) emptyAnalysis

                  testModule =
                    Module
                      ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId "main" ) ) )
                      testModuleName

                  reachableSet =
                    reachable
                      analysis
                      ( Set.singleton
                          Declaration
                            { declModule =
                                testModule
                            , declOccName =
                                mkOccName varName "root"
                            }
                      )

                  dead =
                    Set.filter
                      ( \d -> declModule d == testModule )
                      ( allDeclarations analysis Set.\\ reachableSet )

                unless
                  ( Set.null dead )
                  ( for_ dead \d ->
                      liftIO ( fail ( declarationStableName d <> " is dead, but should be alive" ) )
                  )

-- | Recursively search for .hie files in given directory
getTestCasesIn :: FilePath -> IO [FilePath]
getTestCasesIn path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <-
        doesFileExist path

      if isFile && "hs" `isExtensionOf` path
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

              withCurrentDirectory path ( foldMap getTestCasesIn cnts )

            else
              return []

    else
      return []
