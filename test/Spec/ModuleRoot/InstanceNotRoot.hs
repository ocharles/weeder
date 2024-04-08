{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Spec.ModuleRoot.InstanceNotRoot (C(..), T(..)) where

class C a where
  method :: a -> a

data T = T

instance C T where
  method = id
