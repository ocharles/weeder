{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

-- | This module provides an entry point to the Weeder executable.

module Weeder.Main ( main, mainWithConfig ) where

-- base
import Control.Monad ( guard, unless, when )
import Control.Monad.IO.Class ( liftIO )
import Data.Bool
import Data.Foldable
import Data.List ( isSuffixOf )
import Data.Version ( showVersion )
import System.Exit ( exitFailure )

-- containers
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- text
import qualified Data.Text as T

-- dhall
import qualified Dhall

-- directory
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory )

-- filepath
import System.FilePath ( isExtensionOf )

-- ghc
import GHC.Iface.Ext.Binary ( HieFileResult( HieFileResult, hie_file_result ), NameCacheUpdater( NCU ), readHieFileWithVersion )
import GHC.Iface.Ext.Types ( HieFile( hie_hs_file ), hieVersion )
import GHC.Unit.Module ( moduleName, moduleNameString )
import GHC.Types.Name.Cache ( initNameCache, NameCache )
import GHC.Types.Name ( occNameString )
import GHC.Types.SrcLoc ( RealSrcLoc, realSrcSpanStart, srcLocLine )
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )

-- regex-tdfa
import Text.Regex.TDFA ( (=~) )

-- optparse-applicative
import Options.Applicative

-- transformers
import Control.Monad.Trans.State.Strict ( execStateT )

-- weeder
import Weeder
import Weeder.Config
import Paths_weeder (version)


-- | Parse command line arguments and into a 'Config' and run 'mainWithConfig'.
main :: IO ()
main = do
  (configExpr, hieExt, hieDirectories, requireHsFiles) <-
    execParser $
      info (optsP <**> helper <**> versionP) mempty

  Dhall.input config configExpr
    >>= mainWithConfig hieExt hieDirectories requireHsFiles
  where
    optsP = (,,,)
        <$> strOption
            ( long "config"
                <> help "A Dhall expression for Weeder's configuration. Can either be a file path (a Dhall import) or a literal Dhall expression."
                <> value "./weeder.dhall"
                <> metavar "<weeder.dhall>"
                <> showDefaultWith T.unpack
            )
        <*> strOption
            ( long "hie-extension"
                <> value ".hie"
                <> help "Extension of HIE files"
                <> showDefault
            )
        <*> many (
            strOption
                ( long "hie-directory"
                    <> help "A directory to look for .hie files in. Maybe specified multiple times. Default ./."
                )
            )
        <*> switch
              ( long "require-hs-files"
                  <> help "Requires that all .hie files have matching .hs files. This can help deal with skipping .hie files for Haskell modules that have since been removed"
              )

    versionP = infoOption ( "weeder version "
                            <> showVersion version
                            <> "\nhie version "
                            <> show hieVersion )
        ( long "version" <> help "Show version" )


-- | Run Weeder in the current working directory with a given 'Config'.
--
-- This will recursively find all files with the given extension in the given directories, perform
-- analysis, and report all unused definitions according to the 'Config'.
mainWithConfig :: String -> [FilePath] -> Bool -> Config -> IO ()
mainWithConfig hieExt hieDirectories requireHsFiles Config{ rootPatterns, typeClassRoots } = do
  hieFilePaths <-
    concat <$>
      traverse ( getFilesIn hieExt )
        ( if null hieDirectories
          then ["./."]
          else hieDirectories
        )

  hsFilePaths <-
    if requireHsFiles
      then getFilesIn ".hs" "./."
      else pure []

  nameCache <- do
    uniqSupply <- mkSplitUniqSupply 'z'
    return ( initNameCache uniqSupply [] )

  analysis <-
    flip execStateT emptyAnalysis do
      for_ hieFilePaths \hieFilePath -> do
        hieFileResult <- liftIO ( readCompatibleHieFileOrExit nameCache hieFilePath )
        let hsFileExists = any ( hie_hs_file hieFileResult `isSuffixOf` ) hsFilePaths
        when (requireHsFiles ==> hsFileExists) do
          analyseHieFile hieFileResult

  let
    roots =
      Set.filter
        ( \d ->
            any
              ( ( moduleNameString ( moduleName ( declModule d ) ) <> "." <> occNameString ( declOccName d ) ) =~ )
              rootPatterns
        )
        ( allDeclarations analysis )

    reachableSet =
      reachable
        analysis
        ( Set.map DeclarationRoot roots <> bool mempty ( Set.map DeclarationRoot ( implicitRoots analysis ) ) typeClassRoots )

    dead =
      allDeclarations analysis Set.\\ reachableSet

    warnings =
      Map.unionsWith (++) $
      foldMap
        ( \d ->
            fold $ do
              moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )
              spans <- Map.lookup d ( declarationSites analysis )
              guard $ not $ null spans
              let starts = map realSrcSpanStart $ Set.toList spans
              return [ Map.singleton moduleFilePath ( liftA2 (,) starts (pure d) ) ]
        )
        dead

  for_ ( Map.toList warnings ) \( path, declarations ) ->
    for_ declarations \( start, d ) ->
      putStrLn $ showWeed path start d

  unless ( null warnings ) exitFailure

showWeed :: FilePath -> RealSrcLoc -> Declaration -> String
showWeed path start d =
  path <> ":" <> show ( srcLocLine start ) <> ": "
    <> occNameString ( declOccName d)


-- | Recursively search for files with the given extension in given directory
getFilesIn
  :: String
  -- ^ Only files with this extension are considered
  -> FilePath
  -- ^ Directory to look in
  -> IO [FilePath]
getFilesIn ext path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <-
        doesFileExist path

      if isFile && ext `isExtensionOf` path
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

              withCurrentDirectory path ( foldMap ( getFilesIn ext ) cnts )

            else
              return []

    else
      return []


-- | Read a .hie file, exiting if it's an incompatible version.
readCompatibleHieFileOrExit :: NameCache -> FilePath -> IO HieFile
readCompatibleHieFileOrExit nameCache path = do
  res <- readHieFileWithVersion (\(v, _) -> v == hieVersion) (NCU (\f -> return $ snd $ f nameCache)) path
  case res of
    Right HieFileResult{ hie_file_result } ->
      return hie_file_result
    Left ( v, _ghcVersion ) -> do
      putStrLn $ "incompatible hie file: " <> path
      putStrLn $ "    this version of weeder was compiled with GHC version "
               <> show hieVersion
      putStrLn $ "    the hie files in this project were generated with GHC version "
               <> show v
      putStrLn $ "    weeder must be built with the same GHC version"
               <> " as the project it is used on"
      exitFailure



infixr 5 ==>


-- | An infix operator for logical implication
(==>) :: Bool -> Bool -> Bool
True  ==> x = x
False ==> _ = True

