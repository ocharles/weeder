module UnitTests.Weeder.ConfigSpec (tests) where

import Weeder.Config
import qualified TOML
import qualified Data.Text as T
import Test.Tasty.HUnit
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = 
  testGroup "Weeder.Config"
    [ testCase "configToToml" configToTomlTests ]

configToTomlTests :: Assertion
configToTomlTests =
  let cf = Config
        { rootPatterns = mempty
        , typeClassRoots = True
        , rootInstances = [InstanceOnly "Quux\\\\[\\]", ClassOnly "[\\[\\\\[baz" <> ModuleOnly "[Quuux]", InstanceOnly "[\\[\\\\[baz" <> ClassOnly "[Quuux]" <> ModuleOnly "[Quuuux]"]
        , unusedTypes = True
        , rootModules = ["Foo\\.Bar", "Baz"]
        }
      cf' = T.pack $ configToToml cf
   in TOML.decode cf' `shouldBe` Right cf
