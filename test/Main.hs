import qualified Weeder.Main
import qualified Weeder
import qualified TOML
import qualified Spec

import Algebra.Graph.Export.Dot
import GHC.Types.Name.Occurrence (occNameString)
import System.Directory
import System.Environment (getArgs, withArgs)
import System.FilePath
import System.Process
import System.IO.Silently (hCapture_)
import System.IO (stdout, stderr, hPrint)
import Test.Hspec
import Control.Monad (zipWithM_, when)
import Control.Exception ( throwIO, IOException, handle )
import Data.Maybe (isJust)
import Data.List (find, sortOn)

main :: IO ()
main = do
  args <- getArgs
  testOutputFiles <- fmap sortTests discoverIntegrationTests
  let hieDirectories = map (dropExtension . snd) testOutputFiles
      drawDots = mapM_ (drawDot . (<.> ".dot")) hieDirectories
      graphviz = "--graphviz" `elem` args
  withArgs (filter (/="--graphviz") args) $
    hspec $ afterAll_ (when graphviz drawDots) $ do
      describe "Weeder.Main" $
        describe "mainWithConfig" $
          zipWithM_ (uncurry integrationTestSpec) testOutputFiles hieDirectories
      Spec.spec
  where
    -- Draw a dotfile via graphviz
    drawDot f = callCommand $ "dot -Tpng " ++ f ++ " -o " ++ (f -<.> ".png")
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
integrationTestSpec :: Maybe FilePath -> FilePath -> FilePath -> Spec
integrationTestSpec failingFile stdoutFile hieDirectory = do
  it (integrationTestText ++ hieDirectory) $ do
    expectedOutput <- readFile stdoutFile
    actualOutput <- integrationTestOutput hieDirectory
    case failingFile of
      Just f -> do
        failingOutput <- readFile f
        actualOutput `shouldNotBe` expectedOutput
        actualOutput `shouldBe` failingOutput
      Nothing -> 
        actualOutput `shouldBe` expectedOutput
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
integrationTestOutput :: FilePath -> IO String
integrationTestOutput hieDirectory = hCapture_ [stdout] $ do
  isEmpty <- not . any (".hie" `isExtensionOf`) <$> listDirectory hieDirectory
  when isEmpty $ fail "No .hie files found in directory, this is probably unintended"
  (_, analysis) <-
    TOML.decodeFile configExpr
      >>= either throwIO pure
      >>= Weeder.Main.mainWithConfig ".hie" [hieDirectory] True
  let graph = Weeder.dependencyGraph analysis
      graph' = export (defaultStyle (occNameString . Weeder.declOccName)) graph
  handle (\e -> hPrint stderr (e :: IOException)) $
    writeFile (hieDirectory <.> ".dot") graph'
  where
    configExpr = hieDirectory <.> ".toml"
