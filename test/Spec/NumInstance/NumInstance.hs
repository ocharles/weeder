{-# OPTIONS_GHC -Wno-missing-methods #-}
module Spec.NumInstance.NumInstance where

data Modulo2 = Zero | One

instance Num Modulo2 where
  (+) = add
  -- leave the rest undefined

-- add should not be detected as unused
add :: Modulo2 -> Modulo2 -> Modulo2
add One One = Zero
add Zero n = n
add n Zero = n

two :: Modulo2
two = One + One
