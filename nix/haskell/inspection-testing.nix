{ mkDerivation, base, containers, ghc, mtl, stdenv
, template-haskell, transformers
}:
mkDerivation {
  pname = "inspection-testing";
  version = "0.4.2.2";
  sha256 = "d78fcf2129ee1bccd184069e98deacaf7f41afaa292f8aa082a6477353faf7ae";
  libraryHaskellDepends = [
    base containers ghc mtl template-haskell transformers
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/nomeata/inspection-testing";
  description = "GHC plugin to do inspection testing";
  license = stdenv.lib.licenses.mit;
}
