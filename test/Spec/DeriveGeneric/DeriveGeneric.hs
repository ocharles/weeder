{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Spec.DeriveGeneric.DeriveGeneric where

import GHC.Generics
import Data.Aeson

newtype T = MkT Bool
  -- Generic and ToJSON must not be detected as unused
  -- but FromJSON should be detected as unused
  deriving (Generic, ToJSON, FromJSON)

t :: Value
t = toJSON $ MkT True
