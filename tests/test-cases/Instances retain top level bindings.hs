module Test () where

alive :: String
alive = "Alive"

data T = T

instance Show T where
  show T = alive
