{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Weeder.Config ( Config(..) ) where

-- containers
import Data.Set ( Set )

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
  , rootClasses :: Set String
    -- ^ All instances of type classes matching these regular expressions will
    -- be added to the root set. Note that this does not mark the class itself
    -- as a root, so if the class has no instances then it will not be made
    -- reachable.
  , rootInstances :: Set String
    -- ^ All instances with types matching these regular expressions will 
    -- be added to the root set.
  , unusedTypes :: Bool
    -- ^ Toggle to look for and output unused types. Type family instances will
    -- be marked as implicit roots.
  }

instance TOML.DecodeTOML Config where
  tomlDecoder = do
    rootPatterns <- TOML.getField "roots"
    typeClassRoots <- TOML.getField "type-class-roots"
    rootClasses <- TOML.getFieldOr mempty "root-classes"
    rootInstances <- TOML.getFieldOr mempty "root-instances"
    unusedTypes <- TOML.getFieldOr False "unused-types"

    return Config{..}
