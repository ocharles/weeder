module Weeder.ConfigSpec (spec) where

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
        , rootClasses = Set.fromList [(Nothing, Just "Baz"), (Just "foo\\\\[bar]", Just "Bar\\\\.foo"), (Nothing, Nothing)]
        , rootInstances = Set.fromList [(Nothing, Just "Quux\\\\[\\]"), (Just "[\\[\\\\[baz", Just "[Quuux]"), (Just "a", Nothing)]
        }
      cf' = T.pack $ configToToml cf
   in TOML.decode cf' == Right cf
