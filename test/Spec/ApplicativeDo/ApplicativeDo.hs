{-# LANGUAGE ApplicativeDo #-}

module Spec.ApplicativeDo.ApplicativeDo where


newtype Foo a = Foo a


instance Functor Foo where
  fmap f (Foo a) = Foo (f a)


instance Applicative Foo where
  pure = Foo
  Foo f <*> Foo a = Foo (f a)


root :: Foo Int
root = do
  a <- Foo 1
  b <- Foo 2
  pure (a + b)
