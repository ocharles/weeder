{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Weeder.Config ( Config(..), codec ) where

-- containers
import Data.Set ( Set )

-- toml
import qualified Toml


-- | Configuration for Weeder analysis.
data Config = Config
  { rootPatterns :: Set String
    -- ^ Any declarations matching these regular expressions will be added to
    -- the root set.
  , typeClassRoots :: Bool
    -- ^ If True, consider all declarations in a type class as part of the root
    -- set. Weeder is currently unable to identify whether or not a type class
    -- instance is used - enabling this option can prevent false positives.
  }


-- | A TOML codec for 'Config'.
codec :: Toml.TomlCodec Config
codec =
  Config
    <$> Toml.arraySetOf Toml._String "roots" Toml..= rootPatterns
    <*> Toml.bool "type-class-roots" Toml..= typeClassRoots
