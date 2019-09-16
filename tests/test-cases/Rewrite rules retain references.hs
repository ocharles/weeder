{-# language DefaultSignatures #-}

module Test ( foo ) where

{-# noinline foo #-}
foo :: Bool -> Int
foo _ = 42

bar :: Int
bar = 42

{-# RULES

"test rule"    foo True = bar

#-}
