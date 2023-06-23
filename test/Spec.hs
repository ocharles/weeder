import qualified Weeder.Main
import qualified Weeder
import qualified TOML

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

main :: IO ()
main = do
  args <- getArgs
  stdoutFiles <- discoverIntegrationTests
  let hieDirectories = map dropExtension stdoutFiles
      drawDots = mapM_ (drawDot . (<.> ".dot")) hieDirectories
      graphviz = "--graphviz" `elem` args
  withArgs (filter (/="--graphviz") args) $
    hspec $ afterAll_ (when graphviz drawDots) $ do
      describe "Weeder.Main" $
        describe "mainWithConfig" $
          zipWithM_ integrationTestSpec stdoutFiles hieDirectories
  where
    -- Draw a dotfile via graphviz
    drawDot f = callCommand $ "dot -Tpng " ++ f ++ " -o " ++ (f -<.> ".png")

-- | Run weeder on hieDirectory, comparing the output to stdoutFile
-- The directory containing hieDirectory must also have a .toml file
-- with the same name as hieDirectory
integrationTestSpec :: FilePath -> FilePath -> Spec
integrationTestSpec stdoutFile hieDirectory = do
  it ("produces the expected output for " ++ hieDirectory) $ do
    expectedOutput <- readFile stdoutFile
    actualOutput <- integrationTestOutput hieDirectory
    actualOutput `shouldBe` expectedOutput

-- | Returns detected .stdout files in ./test/Spec
discoverIntegrationTests :: IO [FilePath]
discoverIntegrationTests = do
  contents <- listDirectory "./test/Spec"
  pure . map ("./test/Spec" </>) $ filter (".stdout" `isExtensionOf`) contents

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
