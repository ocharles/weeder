{-# LANGUAGE LambdaCase #-}
import qualified Weeder.Config
import qualified Weeder.Main
import qualified Dhall
import qualified Data.Text as T

import System.Directory
import System.FilePath
import System.IO.Silently (hCapture_)
import System.IO (stdout)
import Test.Hspec
import Control.Monad (zipWithM_, when)
import Control.Exception (handle)
import System.Exit (ExitCode(..))

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
integrationTestOutput :: FilePath -> IO String
integrationTestOutput hieDirectory = hCapture_ [stdout] $ do
  isEmpty <- not . any (".hie" `isExtensionOf`) <$> listDirectory hieDirectory
  when isEmpty $ fail "No .hie files found in directory, this is probably unintended"
  ignoreExit $
    Dhall.input Weeder.Config.config (T.pack dhallFile) >>= 
      Weeder.Main.mainWithConfig ".hie" [hieDirectory] True
  where 
    dhallFile = hieDirectory <.> ".dhall"

ignoreExit :: IO () -> IO ()
ignoreExit = handle (\case ExitFailure _ -> pure (); ExitSuccess -> pure ())
