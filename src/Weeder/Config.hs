{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Weeder.Config 
  ( Config(..)
  , configToToml
  , prop_configToToml
  , defaultConfig 
  ) 
   where

-- base
import Data.Char (toLower)
import Data.List (intersperse)

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set

-- text
import qualified Data.Text as T

-- toml-reader
import qualified TOML


-- | Configuration for Weeder analysis.
data Config = Config
  { rootPatterns :: Set String
    -- ^ Any declarations matching these regular expressions will be added to
    -- the root set.
  , typeClassRoots :: Bool
    -- ^ If True, consider all declarations in a type class as part of the root
    -- set. Weeder is currently unable to identify whether or not a type class
    -- instance is used - enabling this option can prevent false positives.
  , rootClasses :: Set (Maybe String, String)
    -- ^ All instances of type classes matching these regular expressions will
    -- be added to the root set. Note that this does not mark the class itself
    -- as a root, so if the class has no instances then it will not be made
    -- reachable.
  , rootInstances :: Set (Maybe String, String)
    -- ^ All instances with types matching these regular expressions will 
    -- be added to the root set.
  } deriving Eq


defaultConfig :: Config
defaultConfig = Config
  { rootPatterns = Set.fromList [ "Main.main", "^Paths_.*"]
  , typeClassRoots = False
  , rootClasses = mempty
  , rootInstances = mempty
  }


instance TOML.DecodeTOML Config where
  tomlDecoder = do
    rootPatterns <- TOML.getFieldOr (rootPatterns defaultConfig) "roots"
    typeClassRoots <- TOML.getFieldOr (typeClassRoots defaultConfig) "type-class-roots"
    rootClasses <- TOML.getFieldOr (rootClasses defaultConfig) "root-classes"
    rootInstances <- TOML.getFieldOr (rootInstances defaultConfig) "root-instances"

    return Config{..}


configToToml :: Config -> String
configToToml Config{..}
  = unlines . intersperse mempty $
      [ "roots = " ++ show (Set.toList rootPatterns)
      , "type-class-roots = " ++ map toLower (show typeClassRoots)
      , "root-classes = " ++ show (Set.toList rootClasses)
      , "root-instances = " ++ show (Set.toList rootInstances)
      ]


-- | >>> prop_configToToml
-- True
prop_configToToml :: Bool
prop_configToToml =
  let cf = Config
        { rootPatterns = mempty
        , typeClassRoots = True
        , rootClasses = Set.fromList [(Nothing, "baz"), (Just "Foo\\\\.Bar", "quux")]
        , rootInstances = Set.fromList [(Nothing, "quux\\\\[\\]"), (Just "Baz", "[quuux]")]
        }
      cf' = T.pack $ configToToml cf
   in TOML.decode cf' == Right cf
