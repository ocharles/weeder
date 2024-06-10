{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Spec.Types.Usages where

-- weeder
import Spec.Types.Types


numberUsage :: forall a. Number -> Number
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
