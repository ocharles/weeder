{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Spec.OverloadedStringsNoSig.OverloadedStringsNoSig where

import Data.String ( IsString(fromString) )

newtype BetterString = BetterString String

-- $fIsStringBetterString should not be detected as unused
instance IsString BetterString where
  fromString = BetterString

f :: BetterString -> Char
f = const 'a'

-- Adding a type signature to any of the following causes that binding to no
-- longer depend on $fIsStringBetterString

root = f "Hello World" -- evidence variable usage present (GHC 9.4.4), 

root' = f ("Hello World" :: BetterString) -- also no problem here

root'' = "Hello World" :: BetterString -- nor here
