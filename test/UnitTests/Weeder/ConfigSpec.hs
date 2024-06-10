module UnitTests.Weeder.ConfigSpec (tests) where

-- hspec-expectations
import Test.Hspec.Expectations (shouldBe)

-- tasty
import Test.Tasty (TestTree, testGroup)

-- tasty-hunit
import Test.Tasty.HUnit

-- text
import qualified Data.Text as T

-- toml-reader
import qualified TOML

-- weeder
import Weeder.Config


tests :: TestTree
tests =
  testGroup
    "Weeder.Config"
    [testCase "configToToml" configToTomlTests]


configToTomlTests :: Assertion
configToTomlTests =
  let cf =
        Config
          { rootPatterns = mempty
          , typeClassRoots = True
          , rootInstances = [InstanceOnly "Quux\\\\[\\]", ClassOnly "[\\[\\\\[baz" <> ModuleOnly "[Quuux]", InstanceOnly "[\\[\\\\[baz" <> ClassOnly "[Quuux]" <> ModuleOnly "[Quuuux]"]
          , unusedTypes = True
          }
      cf' = T.pack $ configToToml cf
   in TOML.decode cf' `shouldBe` Right cf
