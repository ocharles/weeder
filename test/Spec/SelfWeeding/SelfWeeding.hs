module Spec.SelfWeeding.SelfWeeding where

-- A single reachable root and a helper it uses, so there are no dead
-- declarations: the only weeds in this test come from the configuration
-- itself (a root pattern, a root-instance and a root-module that match
-- nothing).

root :: Int
root = helper + 1

helper :: Int
helper = 41
