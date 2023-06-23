{-# LANGUAGE DataKinds, KindSignatures, 
            FunctionalDependencies, FlexibleInstances,
            OverloadedLabels, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Spec.OverloadedLabels.OverloadedLabels where
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)

data Label (l :: Symbol) = Get

class Has a l b | a l -> b where
  from :: a -> Label l -> b

data Point = Point Int Int -- odd behaviour with dependencies between Point and Int

instance Has Point "x" Int where from (Point x _) _ = x
instance Has Point "y" Int where from (Point _ y) _ = y

instance Has a l b => IsLabel l (a -> b) where
  fromLabel x = from x (Get :: Label l)

root :: Int
root = #x (Point 1 2) 
  -- surprisingly OverloadedLabels works perfectly out of the box
