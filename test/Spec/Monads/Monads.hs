{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spec.Monads.Monads where


newtype Identity a = Identity {runIdentity :: a}


instance Functor Identity where
  fmap f (Identity x) = Identity (f x)


instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)


instance Monad Identity where
  return = pure
  Identity x >>= f = f x


newtype Identity' a = Identity' {runIdentity' :: a}


instance Functor Identity' where
  fmap f (Identity' x) = Identity' (f x)


instance Applicative Identity' where
  pure = Identity'
  Identity' f <*> Identity' x = Identity' (f x)


instance Monad Identity' where
  return = pure
  Identity' x >>= f = f x


foo = do
  _x <- Identity 3
  Identity 4


bar :: Identity' Integer -- oh no (the type signature breaks the evidence variables)
bar = do
  _x <- Identity' 3
  Identity' 4
