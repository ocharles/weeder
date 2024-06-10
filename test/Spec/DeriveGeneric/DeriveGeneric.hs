{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Spec.DeriveGeneric.DeriveGeneric where

-- aeson
import Data.Aeson

-- base
import GHC.Generics


newtype T = MkT Bool
  -- Generic and ToJSON must not be detected as unused
  -- but FromJSON should be detected as unused
  deriving
    ( Generic
    , ToJSON
    , FromJSON
    )


t :: Value
t = toJSON $ MkT True
