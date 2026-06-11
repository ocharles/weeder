module Spec.DefaultsNotReported.DefaultsNotReported where

-- This module is kept entirely alive by an explicit `root-modules` entry, so
-- there are no dead declarations. The point of the test is that the *default*
-- `roots` (`Main.main`, `^Paths_.*`) and the default `root-instances`
-- (`IsString`, `IsList`) -- none of which match anything here -- are NOT
-- reported, because they were never configured explicitly. The output is empty.

value :: Int
value = 5
