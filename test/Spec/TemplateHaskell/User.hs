{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Spec.TemplateHaskell.User where

import Spec.TemplateHaskell.TH
import GHC.TypeLits (Nat)

$(threeQ)

newtype T (a :: Nat) = T Int

root :: T [intQQ|1|]
root = T $ $(oneQ) + [intQQ|1|] + quote + f (1 :: Int) + $(twoQ) + three'
  where
    f [intQQ|1|] = 1
    f _ = 1

quote :: Int
[intQQ|2|]
