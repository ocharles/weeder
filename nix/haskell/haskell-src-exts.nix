{ mkDerivation, array, base, containers, directory, filepath
, ghc-prim, happy, mtl, pretty, pretty-show, smallcheck, stdenv
, tasty, tasty-golden, tasty-smallcheck
}:
mkDerivation {
  pname = "haskell-src-exts";
  version = "1.21.1";
  sha256 = "2ec911614325f1e5eef7e5bef15c08b8265931d69da3ce566af109d486453e60";
  libraryHaskellDepends = [ array base ghc-prim pretty ];
  libraryToolDepends = [ happy ];
  testHaskellDepends = [
    base containers directory filepath mtl pretty-show smallcheck tasty
    tasty-golden tasty-smallcheck
  ];
  doCheck = false;
  homepage = "https://github.com/haskell-suite/haskell-src-exts";
  description = "Manipulating Haskell source: abstract syntax, lexer, parser, and pretty-printer";
  license = stdenv.lib.licenses.bsd3;
}
