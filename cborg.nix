{ mkDerivation, aeson, array, base, base-orphans, base16-bytestring
, base64-bytestring, bytestring, containers, deepseq, ghc-prim
, half, integer-gmp, primitive, QuickCheck, random, scientific
, stdenv, tasty, tasty-hunit, tasty-quickcheck, text, vector
}:
mkDerivation {
  pname = "cborg";
  version = "0.2.2.1";
  sha256 = "ba920d368892fe14e048cd6ac4270ce4ea1aea0fb6a4998c5c97fe106e6c6183";
  libraryHaskellDepends = [
    array base bytestring containers deepseq ghc-prim half integer-gmp
    primitive text
  ];
  testHaskellDepends = [
    aeson array base base-orphans base16-bytestring base64-bytestring
    bytestring deepseq half QuickCheck random scientific tasty
    tasty-hunit tasty-quickcheck text vector
  ];
  doCheck = false;
  description = "Concise Binary Object Representation (CBOR)";
  license = stdenv.lib.licenses.bsd3;
}
