{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Spec.Types.Types where


-- this Bounded will not depend on Modulo1 as intended
data Modulo1 = Zero deriving (Bounded)


-- should be reachable via Number
data Modulo2 = Zero' | One'


type Number = Modulo2


newtype Vector a = MkVector (a, a, a) deriving (Functor)


data Record = MkRecord
  { recordField1 :: Int
  , recordField2 :: Double
  }
