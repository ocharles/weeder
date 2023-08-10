{-# LANGUAGE StandaloneDeriving #-}
module Spec.StandaloneDeriving.StandaloneDeriving where

data A = A

deriving instance Show A

data T = T

deriving instance Show T

root :: String
root = show T
