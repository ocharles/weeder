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

planets = [minBound .. (maxBound :: Planet)]

letters = map f [minBound .. maxBound]
  where
    f :: Letter -> String
    f = show

shapes = [minBound .. maxBound] :: [Shape]

colours :: [Colour]
colours = [minBound .. maxBound] :: [Colour] -- breaks
