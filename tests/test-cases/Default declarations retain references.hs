{-# language DefaultSignatures #-}

module Test () where

alive :: String
alive = "Alive"

class C a where
  cmethod :: a -> String
  default cmethod :: a -> String
  cmethod _ = alive

data T

instance C T
