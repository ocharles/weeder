module Main (main) where

import qualified Weeder.Main
import qualified Weeder.Run
import qualified Weeder
import qualified TOML
import qualified UnitTests.Weeder.ConfigSpec

import Data.Maybe
import Algebra.Graph.Export.Dot
import GHC.Types.Name.Occurrence (occNameString)
import System.Directory
import System.Environment (getArgs, withArgs)
import System.FilePath
import System.Process
import System.IO (stderr, hPrint)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Control.Monad (zipWithM_, when)
import Control.Exception ( throwIO, IOException, handle )
import Data.Maybe (isJust)
import Data.List (find, sortOn)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Test.Tasty.Golden

main :: IO ()
main = do
  testOutputFiles <- fmap sortTests discoverIntegrationTests
  let hieDirectories = map (dropExtension . snd) testOutputFiles
  defaultMain $ 
    testGroup "Weeder"
      [ testGroup "Weeder.Run" $
          [ testGroup "runWeeder" $
              zipWith (uncurry integrationTest) 
                testOutputFiles 
                hieDirectories
          ]
      , UnitTests.Weeder.ConfigSpec.tests 
      ]
  where
    -- Sort the output files such that the failing ones go last
    sortTests = sortOn (isJust . fst)

-- | Run weeder on @hieDirectory@, comparing the output to @stdoutFile@.
--
-- The directory containing @hieDirectory@ must also have a @.toml@ file
-- with the same name as @hieDirectory@.
--
-- If @failingFile@ is @Just@, it is used as the expected output instead of
-- @stdoutFile@, and a different failure message is printed if the output
-- matches @stdoutFile@.
integrationTest :: Maybe FilePath -> FilePath -> FilePath -> TestTree
integrationTest failingFile stdoutFile hieDirectory = do
  goldenVsString (integrationTestText ++ hieDirectory) (fromMaybe stdoutFile failingFile) $ 
    integrationTestOutput hieDirectory
  where
    integrationTestText = case failingFile of
      Nothing -> "produces the expected output for "
      Just _ -> "produces the expected (wrong) output for "

-- | Returns detected .failing and .stdout files in ./test/Spec
discoverIntegrationTests :: IO [(Maybe FilePath, FilePath)]
discoverIntegrationTests = do
  contents <- listDirectory testPath
  let stdoutFiles = map (testPath </>) $
        filter (".stdout" `isExtensionOf`) contents
  pure . map (\s -> (findFailing s contents, s)) $ stdoutFiles
    where
      findFailing s = fmap (testPath </>) . find (takeBaseName s <.> ".failing" ==)
      testPath = "./test/Spec"

-- | Run weeder on the given directory for .hie files, returning stdout
-- Also creates a dotfile containing the dependency graph as seen by Weeder
integrationTestOutput :: FilePath -> IO LBS.ByteString
integrationTestOutput hieDirectory = do
  hieFiles <- Weeder.Main.getHieFiles ".hie" [hieDirectory] True
  weederConfig <- TOML.decodeFile configExpr >>= either throwIO pure
  let (weeds, analysis) = Weeder.Run.runWeeder weederConfig hieFiles
      graph = Weeder.dependencyGraph analysis
      graph' = export (defaultStyle (occNameString . Weeder.declOccName)) graph
  handle (\e -> hPrint stderr (e :: IOException)) $
    writeFile (hieDirectory <.> ".dot") graph'
  pure (LBS.fromStrict $ encodeUtf8 $ pack $ unlines $ map Weeder.Run.formatWeed weeds)
  where
    configExpr = hieDirectory <.> ".toml"
