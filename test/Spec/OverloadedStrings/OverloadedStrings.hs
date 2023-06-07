{-# LANGUAGE OverloadedStrings #-}
module Spec.OverloadedStrings.OverloadedStrings where

import Data.String ( IsString(fromString) )

newtype BetterString = BetterString String

-- $fIsStringBetterString should not be detected as unused
instance IsString BetterString where
  fromString = BetterString

-- Thought: this problem might be similar to RebindableSyntax, QualifiedDo, etc
root :: BetterString
root = "Hello World" -- no evidence variable usage here as of GHC 9.4.4
