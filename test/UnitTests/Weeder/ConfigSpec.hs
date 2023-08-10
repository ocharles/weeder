module UnitTests.Weeder.ConfigSpec (spec) where

import Weeder.Config
import qualified Data.Set as Set
import qualified TOML
import qualified Data.Text as T
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = 
  describe "Weeder.Config" $
    describe "configToToml" $
      it "passes prop_configToToml" prop_configToToml

-- >>> prop_configToToml
-- True
prop_configToToml :: Bool
prop_configToToml =
  let cf = Config
        { rootPatterns = mempty
        , typeClassRoots = True
        , rootInstances = Set.fromList [InstanceOnly "Quux\\\\[\\]", ClassOnly "[\\[\\\\[baz" <> ModuleOnly "[Quuux]", InstanceOnly "[\\[\\\\[baz" <> ClassOnly "[Quuux]" <> ModuleOnly "[Quuuux]"]
        , unusedTypes = True
        }
      cf' = T.pack $ configToToml cf
   in TOML.decode cf' == Right cf
