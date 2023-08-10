{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Spec.TypeDataDecl.TypeDataDecl where

data Kind = MkKind

type Number = Int

data Root (l :: Kind) = MkRecord
  { recordField :: Number
  }
