module Spec.TemplateHaskell.TH (intQQ, oneQ, twoQ, two, three, threeQ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

oneQ :: Q Exp
oneQ = pure . LitE $ IntegerL one

one :: Integer
one = 1

two :: Int
two = 2

twoQ :: Q Exp
twoQ = pure . VarE $ mkName "two"

three :: Int
three = 3

threeQ :: Q [Dec]
threeQ = pure [ValD (VarP $ mkName "three'") (NormalB . VarE $ mkName "three") []]

intQQ :: QuasiQuoter
intQQ = QuasiQuoter 
  { quoteExp = pure . LitE . IntegerL . (zero1 +) . read
  , quotePat = pure . LitP . IntegerL . (zero2 +) . read
  , quoteType = pure . LitT . NumTyLit . (zero3 +) . read
  , quoteDec = pure . pure . (\i -> ValD (VarP $ mkName "quote") (NormalB $ LitE $ IntegerL i) []) . (zero4 +) . read
  }

zero1 :: Integer
zero1 = 0

zero2 :: Integer
zero2 = 0

zero3 :: Integer
zero3 = 0

zero4 :: Integer
zero4 = 0
