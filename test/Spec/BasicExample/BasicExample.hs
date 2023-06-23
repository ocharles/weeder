module Spec.BasicExample.BasicExample where

unrelated :: Int
unrelated = 3

dependency :: Int
dependency = 1

root :: Int
root = dependency + 1
