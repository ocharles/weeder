{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Spec.RangeEnum.RangeEnum where

data Planet = Mercury | Venus | Earth
  deriving (Enum, Bounded)

data Letter = A | B | C
  deriving (Enum, Bounded, Show)

data Shape = Circle | Square | Triangle
  deriving (Enum, Bounded)

data Colour = Red | Green | Blue
  deriving (Enum, Bounded)

planets = [minBound .. (maxBound :: Planet)] -- also doesn't break (GHC 9.4.4)

letters = map f [minBound .. maxBound] -- also doesn't break (GHC 9.4.4)
  where
    f :: Letter -> String
    f = show

shapes = [minBound .. maxBound] :: [Shape] -- also doesn't break (GHC 9.4.4)

colours :: [Colour]
colours = [minBound .. maxBound] :: [Colour] -- breaks (GHC 9.4.4)
