{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Spec.OverloadedStrings.OverloadedStrings where

-- base
import Data.String (IsString (fromString))


newtype BetterString = BetterString String


-- $fIsStringBetterString should not be detected as unused
instance IsString BetterString where
  fromString = BetterString


newtype BetterString' = BetterString' String


instance IsString BetterString' where
  fromString = BetterString'


-- Thought: this problem might be similar to RebindableSyntax, QualifiedDo, etc
root :: BetterString
root = "Hello World" -- no evidence variable usage here


root' = "Hello World" :: BetterString' -- evidence usage present
