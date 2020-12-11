{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Weeder.Config ( Config(..), config ) where

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set

-- dhall
import qualified Dhall


-- | Configuration for Weeder analysis.
data Config = Config
  { rootPatterns :: Set String
    -- ^ Any declarations matching these regular expressions will be added to
    -- the root set.
  , typeClassRoots :: Bool
    -- ^ If True, consider all declarations in a type class as part of the root
    -- set. Weeder is currently unable to identify whether or not a type class
    -- instance is used - enabling this option can prevent false positives.
  , ignore :: Set String
    -- ^ Any weeds matching these regular expressions will not be reported. This
    -- is different from 'rootPatterns', which causes the weed to become a root,
    -- while this option is purely about filtering the reporting output.
  }


-- | A Dhall expression decoder for 'Config'.
--
-- This parses Dhall expressions of the type @{ roots : List Text, type-class-roots : Bool }@.
config :: Dhall.Decoder Config
config =
  Dhall.record do
    rootPatterns   <- Set.fromList <$> Dhall.field "roots" ( Dhall.list Dhall.string )
    typeClassRoots <- Dhall.field "type-class-roots" Dhall.bool
    ignore         <- Set.fromList <$> Dhall.field "ignore" ( Dhall.list Dhall.string )

    return Config{..}
