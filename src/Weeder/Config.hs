{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Weeder.Config ( Config(..) ) where

-- containers
import Data.Set ( Set )

-- aeson
import qualified Data.Aeson as Aeson


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

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \ object -> do
    rootPatterns <- object Aeson..: "roots"
    typeClassRoots <- object Aeson..: "type-class-roots"
    return Config{..}
