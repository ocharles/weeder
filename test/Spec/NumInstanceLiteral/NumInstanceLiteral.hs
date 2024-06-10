{-# OPTIONS_GHC -Wno-missing-methods #-}

module Spec.NumInstanceLiteral.NumInstanceLiteral where


data Modulo1 = Zero


-- $fNumModulo1 should not be detected as unused
instance Num Modulo1 where
  fromInteger _ = Zero


-- leave the rest undefined

zero :: Modulo1
zero = 0 -- no evidence usage here at all in the HieAST (9.4.4 and 9.6.1)
