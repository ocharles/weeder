{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

-- | This module provides an entry point to the Weeder executable.

module Weeder.Main ( main, mainWithConfig ) where

-- base
import Control.Monad ( guard, unless )
import Control.Monad.IO.Class ( liftIO )
import Data.Bool
import Data.Foldable
import Data.Version ( showVersion )
import Text.Printf ( printf )
import System.Exit ( exitFailure )

-- bytestring
import qualified Data.ByteString.Char8 as BS

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
import HieBin ( HieFileResult( HieFileResult, hie_file_result ), readHieFileWithVersion )
import HieTypes ( HieFile, hieVersion )
import Module ( moduleName, moduleNameString )
import NameCache ( initNameCache, NameCache )
import OccName ( occNameString )
import SrcLoc ( RealSrcLoc, realSrcSpanStart, srcLocCol, srcLocLine )
import UniqSupply ( mkSplitUniqSupply )

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
  (configExpr, hieDirectories, verbosity) <-
    execParser $
      info (optsP <**> helper <**> versionP) mempty

  Dhall.input config configExpr >>= mainWithConfig hieDirectories verbosity
  where
    optsP = (,,)
        <$> strOption
            ( long "config"
                <> help "A Dhall expression for Weeder's configuration. Can either be a file path (a Dhall import) or a literal Dhall expression."
                <> value "./weeder.dhall"
                <> metavar "<weeder.dhall>"
                <> showDefaultWith T.unpack
            )
        <*> many (
            strOption
                ( long "hie-directory"
                    <> help "A directory to look for .hie files in. Maybe specified multiple times. Default ./."
                )
            )
        <*> flag Verbose Concise (
            long "concise"
              <> help "Print weeds in a more concise format."
            )

    versionP = infoOption ( "weeder version "
                            <> showVersion version
                            <> "\nhie version "
                            <> show hieVersion )
        ( long "version" <> help "Show version" )

data Verbosity = Concise | Verbose
  deriving (Eq, Show)


-- | Run Weeder in the current working directory with a given 'Config'.
--
-- This will recursively find all @.hie@ files in the current directory, perform
-- analysis, and report all unused definitions according to the 'Config'.
mainWithConfig :: [FilePath] -> Verbosity -> Config -> IO ()
mainWithConfig hieDirectories verbosity Config{ rootPatterns, typeClassRoots } = do
  hieFilePaths <-
    concat <$>
      traverse getHieFilesIn
        ( if null hieDirectories
          then ["./."]
          else hieDirectories
        )

  nameCache <- do
    uniqSupply <- mkSplitUniqSupply 'z'
    return ( initNameCache uniqSupply [] )

  analysis <-
    flip execStateT emptyAnalysis do
      for_ hieFilePaths \hieFilePath -> do
        hieFileResult <- liftIO ( readCompatibleHieFileOrExit nameCache hieFilePath )
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
              moduleSource <- Map.lookup ( declModule d ) ( moduleSource analysis )

              spans <- Map.lookup d ( declarationSites analysis )
              guard $ not $ null spans

              let snippets = do
                    srcSpan <- Set.toList spans

                    let start = realSrcSpanStart srcSpan
                    let firstLine = max 0 ( srcLocLine start - 3 )

                    return ( start, take 5 $ drop firstLine $ zip [1..] $ BS.lines moduleSource )

              return [ Map.singleton moduleFilePath ( liftA2 (,) snippets (pure d) ) ]
        )
        dead

  for_ ( Map.toList warnings ) \( path, declarations ) ->
    for_ declarations \( ( start, snippet ), d ) ->
      putStrLn $ showWeed path start snippet d verbosity
  case verbosity of
    Concise -> return () -- wc will show weed count for Concise
    Verbose -> putStrLn $ "Weeds detected: " <> show ( sum ( length <$> warnings ) )

  unless ( null warnings ) exitFailure

showWeed :: FilePath -> RealSrcLoc -> [(Int, BS.ByteString)]
  -> Declaration -> Verbosity -> String
showWeed path start snippet d = \case
  Concise -> unwords [ filename, occNameString ( declOccName d ) ]
  Verbose -> unlines $
    [ unwords [ filename, "error:", occNameString ( declOccName d ), "is unused" ]
    , ""
    ] ++
    [ replicate 4 ' ' <> printf "% 4d" n <> " ┃ " <> BS.unpack line
    | (n, line) <- snippet
    ] ++
    [ ""
    , replicate 4 ' '
      <> "Delete this definition or add ‘"
      <> moduleNameString ( moduleName ( declModule d ) )
      <> "."
      <> occNameString ( declOccName d )
      <> "’ as a root to fix this error."
    , ""
    ]
  where
  filename = foldMap ( <> ":" )
    [ path, show ( srcLocLine start ), show ( srcLocCol start ) ]


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


-- | Read a .hie file, exiting if it's an incompatible version.
readCompatibleHieFileOrExit :: NameCache -> FilePath -> IO HieFile
readCompatibleHieFileOrExit nameCache path = do
  res <- readHieFileWithVersion (\ (v, _) -> v == hieVersion) nameCache path
  case res of
    Right ( HieFileResult{ hie_file_result }, _ ) ->
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
