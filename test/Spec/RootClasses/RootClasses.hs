-- | Test for marking classes as roots
{-# LANGUAGE StandaloneDeriving #-}
module Spec.RootClasses.RootClasses where

data T = MkT deriving (Eq, Show, Enum)

data U = MkU

deriving instance Ord T

data V = MkV

class Bar a where
  bar :: a -> Char

instance Bar V where
  bar = const 'b'
