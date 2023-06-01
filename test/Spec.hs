import qualified Weeder.Config
import qualified Weeder.Main
import qualified Weeder
import qualified Dhall
import qualified Data.Text as T

import Algebra.Graph.Export.Dot
import GHC.Types.Name.Occurrence (occNameString)
import System.Directory
import System.FilePath
import System.IO.Silently (hCapture_)
import System.IO (stdout, stderr, hPrint)
import Test.Hspec
import Control.Monad (zipWithM_, when)
import Control.Exception (IOException)
import Control.Exception.Base (handle)

main :: IO ()
main = do
  stdoutFiles <- discoverIntegrationTests
  let hieDirectories = map dropExtension stdoutFiles
  hspec $
    describe "Weeder.Main" $
      describe "mainWithConfig" $
        zipWithM_ integrationTestSpec stdoutFiles hieDirectories

-- | Run weeder on hieDirectory, comparing the output to stdoutFile
-- The directory containing hieDirectory must also have a .dhall file
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
    Dhall.input Weeder.Config.config (T.pack dhallFile)
      >>= Weeder.Main.mainWithConfig' ".hie" [hieDirectory] True
  let graph = Weeder.dependencyGraph analysis
      graph' = export (defaultStyle (occNameString . Weeder.declOccName)) graph
  handle (\e -> hPrint stderr (e :: IOException)) $ 
    writeFile (hieDirectory <.> ".dot") graph'
  where
    dhallFile = hieDirectory <.> ".dhall"
