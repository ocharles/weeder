{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Spec.TemplateHaskell.User where

import Spec.TemplateHaskell.TH (intQQ, oneQ)
import GHC.TypeLits (Nat)

newtype T (a :: Nat) = T Int

root :: T [intQQ|1|]
root = T $ $(oneQ) + [intQQ|1|] + quote + f (1 :: Int)
  where
    f [intQQ|1|] = 1
    f _ = 1

quote :: Int
[intQQ|2|]
