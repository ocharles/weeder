{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Spec.ModuleRoot.M (root) where

root :: ()
root = dependency

dependency :: ()
dependency = ()

weed :: ()
weed = ()
