{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Spec.Types.Usages where

import Spec.Types.Types

numberUsage :: Number -> Number
numberUsage = id

-- should depend on Modulo1
modulo1Usage = minBound

-- exists to force type inference of modulo1Root to Modulo1
notRoot :: Modulo1
notRoot = modulo1Usage

recordUsage = recordField1

vectorUsage = mapVector (const (0 :: Integer))
  where
    mapVector :: (b -> c) -> Vector b -> Vector c
    mapVector = fmap
