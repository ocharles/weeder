{-# language ApplicativeDo #-}
{-# language ScopedTypeVariables #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}

-- | This module provides an entry point to the Weeder executable.

module Weeder.Main ( main, mainWithConfig, getHieFiles ) where

-- async
import Control.Concurrent.Async ( async, link, ExceptionInLinkedThread ( ExceptionInLinkedThread ) )

-- base
import Control.Exception ( Exception, throwIO, displayException, catches, Handler ( Handler ), SomeException ( SomeException ))
import Control.Concurrent ( getChanContents, newChan, writeChan, setNumCapabilities )
import Data.List
import Control.Monad ( unless, when )
import Data.Foldable
import Data.Maybe ( isJust, catMaybes )
import Data.Version ( showVersion )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( stderr, hPutStrLn )

-- toml-reader
import qualified TOML

-- directory
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory, withCurrentDirectory, pathIsSymbolicLink, getSymbolicLinkTarget )

-- filepath
import System.FilePath ( isExtensionOf )

-- ghc
import GHC.Iface.Ext.Binary ( HieFileResult( HieFileResult, hie_file_result ), readHieFileWithVersion )
import GHC.Iface.Ext.Types ( HieFile( hie_hs_file ), hieVersion )
import GHC.Types.Name.Cache ( initNameCache, NameCache )

-- optparse-applicative
import Options.Applicative

-- text
import qualified Data.Text.IO as T

-- weeder
import Weeder.Run
import Weeder.Config
import Paths_weeder (version)


-- | Each exception corresponds to an exit code.
data WeederException 
  = ExitNoHieFilesFailure
  | ExitHieVersionFailure 
      FilePath -- ^ Path to HIE file
      Integer -- ^ HIE file's header version
  | ExitConfigFailure
      String -- ^ Error message
  | ExitWeedsFound
  deriving Show


weederExitCode :: WeederException -> ExitCode
weederExitCode = \case
  ExitWeedsFound -> ExitFailure 228
  ExitHieVersionFailure _ _ -> ExitFailure 2
  ExitConfigFailure _ -> ExitFailure 3
  ExitNoHieFilesFailure -> ExitFailure 4


instance Exception WeederException where
  displayException = \case
    ExitNoHieFilesFailure -> noHieFilesFoundMessage
    ExitHieVersionFailure path v -> hieVersionMismatchMessage path v
    ExitConfigFailure s -> s
    ExitWeedsFound -> mempty
    where

      noHieFilesFoundMessage =  
        "No HIE files found: check that the directory is correct "
        <> "and that the -fwrite-ide-info compilation flag is set."

      hieVersionMismatchMessage path v = unlines
        [ "incompatible hie file: " <> path
        , "    this version of weeder was compiled with GHC version "
          <> show hieVersion
        , "    the hie files in this project were generated with GHC version "
          <> show v
        , "    weeder must be built with the same GHC version"
          <> " as the project it is used on"
        ]


-- | Convert 'WeederException' to the corresponding 'ExitCode' and emit an error 
-- message to stderr.
--
-- Additionally, unwrap 'ExceptionInLinkedThread' exceptions: this is for
-- 'getHieFiles'.
handleWeederException :: IO a -> IO a
handleWeederException a = catches a handlers 
  where
    handlers = [ Handler rethrowExits
               , Handler unwrapLinks
               ]
    rethrowExits w = do
      hPutStrLn stderr (displayException w)
      exitWith (weederExitCode w)
    unwrapLinks (ExceptionInLinkedThread _ (SomeException w)) =
      throwIO w


data CLIArguments = CLIArguments
  { configPath :: FilePath
  , hieExt :: String
  , hieDirectories :: [FilePath]
  , requireHsFiles :: Bool
  , writeDefaultConfig :: Bool
  , noDefaultFields :: Bool
  , capabilities :: Maybe Int
  }


parseCLIArguments :: Parser CLIArguments
parseCLIArguments = do
    configPath <- strOption
        ( long "config"
            <> help "A file path for Weeder's configuration."
            <> value "./weeder.toml"
            <> metavar "<weeder.toml>"
        )
    hieExt <- strOption
        ( long "hie-extension"
            <> value ".hie"
            <> help "Extension of HIE files"
            <> showDefault
        )
    hieDirectories <- many (
        strOption
            ( long "hie-directory"
                <> help "A directory to look for .hie files in. Maybe specified multiple times. Default ./."
            )
        )
    requireHsFiles <- switch
          ( long "require-hs-files"
              <> help "Skip stale .hie files with no matching .hs modules"
          )
    writeDefaultConfig <- switch
          ( long "write-default-config"
              <> help "Write a default configuration file if the one specified by --config does not exist"
          )
    noDefaultFields <- switch
          ( long "no-default-fields"
              <> help "Do not use default field values for missing fields in the configuration."
          )
    capabilities <- nParser <|> jParser
    pure CLIArguments{..}
    where
      jParser = Just <$> option auto
          ( short 'j'
              <> value 1
              <> help "Number of cores to use."
              <> showDefault)
      nParser = flag' Nothing
          ( short 'N'
              <> help "Use all available cores."
          )


-- | Parse command line arguments and into a 'Config' and run 'mainWithConfig'.
--
-- Exits with one of the listed Weeder exit codes on failure.
main :: IO ()
main = handleWeederException do
  CLIArguments{..} <-
    execParser $
      info (parseCLIArguments <**> helper <**> versionP) mempty

  traverse_ setNumCapabilities capabilities

  configExists <-
    doesFileExist configPath

  unless (writeDefaultConfig ==> configExists) do
    hPutStrLn stderr $ "Did not find config: wrote default config to " ++ configPath
    writeFile configPath (configToToml defaultConfig)

  decodeConfig noDefaultFields configPath
    >>= either throwConfigError pure
    >>= mainWithConfig hieExt hieDirectories requireHsFiles
  where
    throwConfigError e =
      throwIO $ ExitConfigFailure (displayException e)

    decodeConfig noDefaultFields =
      if noDefaultFields
        then fmap (TOML.decodeWith decodeNoDefaults) . T.readFile
        else TOML.decodeFile

    versionP = infoOption ( "weeder version "
                            <> showVersion version
                            <> "\nhie version "
                            <> show hieVersion )
        ( long "version" <> help "Show version" )


-- | Run Weeder in the current working directory with a given 'Config'.
--
-- This will recursively find all files with the given extension in the given directories, perform
-- analysis, and report all unused definitions according to the 'Config'.
--
-- Exits with one of the listed Weeder exit codes on failure.
mainWithConfig :: String -> [FilePath] -> Bool -> Config -> IO ()
mainWithConfig hieExt hieDirectories requireHsFiles weederConfig = handleWeederException do
  hieFiles <-
    getHieFiles hieExt hieDirectories requireHsFiles

  when (null hieFiles) $ throwIO ExitNoHieFilesFailure

  let
    (weeds, _) =
      runWeeder weederConfig hieFiles

  mapM_ (putStrLn . formatWeed) weeds

  unless (null weeds) $ throwIO ExitWeedsFound


-- | Find and read all .hie files in the given directories according to the given parameters,
-- exiting if any are incompatible with the current version of GHC.
-- The .hie files are returned as a lazy stream in the form of a list.
--
-- Will rethrow exceptions as 'ExceptionInLinkedThread' to the calling thread.
getHieFiles :: String -> [FilePath] -> Bool -> IO [HieFile]
getHieFiles hieExt hieDirectories requireHsFiles = do
  hieFilePaths :: [FilePath] <-
    concat <$>
      traverse ( getFilesIn hieExt )
        ( if null hieDirectories
          then ["./."]
          else hieDirectories
        )

  hsFilePaths :: [FilePath] <-
    if requireHsFiles
      then getFilesIn ".hs" "./."
      else pure []

  hieFileResultsChan <- newChan

  nameCache <-
    initNameCache 'z' []

  a <- async $ handleWeederException do
    readHieFiles nameCache hieFilePaths hieFileResultsChan hsFilePaths
    writeChan hieFileResultsChan Nothing
 
  link a

  catMaybes . takeWhile isJust <$> getChanContents hieFileResultsChan

  where

    readHieFiles nameCache hieFilePaths hieFileResultsChan hsFilePaths =
      for_ hieFilePaths \hieFilePath -> do
        hieFileResult <-
          readCompatibleHieFileOrExit nameCache hieFilePath
        let hsFileExists = any ( hie_hs_file hieFileResult `isSuffixOf` ) hsFilePaths
        when (requireHsFiles ==> hsFileExists) $
          writeChan hieFileResultsChan (Just hieFileResult)


-- | Recursively search for files with the given extension in given directory
getFilesIn
  :: String
  -- ^ Only files with this extension are considered
  -> FilePath
  -- ^ Directory to look in
  -> IO [FilePath]
getFilesIn ext relRoot = do
  -- this implementation fixes https://github.com/ocharles/weeder/issues/163, but there are
  -- several alternatives that either *may* fix the same problem or *should* fix it, and we
  -- could help them upstream. in particular:
  --
  -- - https://hackage.haskell.org/package/Glob-0.10.2/docs/System-FilePath-Glob.html
  --   (most promising candidate i've found, but i haven't checked it yet.)
  -- - https://hackage.haskell.org/package/extra-1.7.12/docs/System-Directory-Extra.html
  --   (checked: package `extra` needs fixing, we could copy `getFilesIn` upstream).
  -- - https://hackage.haskell.org/package/filepattern-0.1.3/docs/System-FilePattern-Directory.html#v:getDirectoryFilesIgnoreSlow
  --   (haven't checked, but the docs about how inconsistencies on case-sensitive file systems are ok may be a red flag?)

  let -- call `canonicalizePath` and resolve sym links (up to a generous recursion depth).
      hyperCanonicalizePath :: Int -> FilePath -> IO FilePath
      hyperCanonicalizePath limit@30 _ = error $ "recursion limit of " <> show limit <> " reached, giving up!"
      hyperCanonicalizePath n x0 = do
        x1 <- canonicalizePath x0
        x2 <- pathIsSymbolicLink x1
        x3 <- if x2 then hyperCanonicalizePath (n+1) =<< getSymbolicLinkTarget x1 else pure x1
        pure x3

  root <- hyperCanonicalizePath 0 relRoot

  let -- iterate over one file name in the search path(s).
      go :: [FilePath] -> FilePath -> IO [FilePath]
      go alreadyTraversed relPath = do
        path <- hyperCanonicalizePath 0 relPath

        -- if file, right extension, and not a super-directory: add to output
        dfx <- doesFileExist path
        let addPlease = dfx && ext `isExtensionOf` path && root `isPrefixOf` path

        -- if directory, not already traversed, and not super-directory: recurse
        ddx <- doesDirectoryExist path
        let recursePlease = ddx && path `notElem` alreadyTraversed && root `isPrefixOf` path

        if addPlease
          then pure [path]
          else if recursePlease
            then do
              withCurrentDirectory path $ do
                subdirs <- listDirectory "./."
                result <- traverse (go (path : alreadyTraversed)) subdirs
                pure $ mconcat result
            else do
              pure []

  go [] root 


-- | Read a .hie file, exiting if it's an incompatible version.
readCompatibleHieFileOrExit :: NameCache -> FilePath -> IO HieFile
readCompatibleHieFileOrExit nameCache path = do
  res <- readHieFileWithVersion (\(v, _) -> v == hieVersion) nameCache path
  case res of
    Right HieFileResult{ hie_file_result } ->
      return hie_file_result
    Left ( v, _ghcVersion ) ->
      throwIO $ ExitHieVersionFailure path v


infixr 5 ==>


-- | An infix operator for logical implication
(==>) :: Bool -> Bool -> Bool
True  ==> x = x
False ==> _ = True
