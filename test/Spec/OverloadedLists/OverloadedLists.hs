{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.OverloadedLists.OverloadedLists where

-- base
import GHC.IsList (IsList (..))


data BetterList x = Nil | Cons x (BetterList x)


instance IsList (BetterList x) where
  type Item (BetterList x) = x
  fromList = foldr Cons Nil
  toList Nil = []
  toList (Cons x xs) = x : toList xs


root :: BetterList Int
root = [1, 2, 3]
