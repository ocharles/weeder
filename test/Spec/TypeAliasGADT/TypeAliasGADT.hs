{-# LANGUAGE GADTs #-}

module Spec.TypeAliasGADT.TypeAliasGADT where


-- This is a false positive when the GADT language extension
-- is enabled and we do not consider type signatures
type Secret = String


data A = MkA String Int


root :: Secret -> Int -> Secret
root secret a =
  let _params = MkA mempty a
   in secret
