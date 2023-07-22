{-# LANGUAGE TypeFamilies #-}
-- | NOTE: the .stdout file for this test is a placeholder
module Spec.TypeFamilies.TypeFamilies where

type family Family a

type instance Family Int = Bool

type instance Family Bool = Int
