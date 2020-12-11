{ mkDerivation, aeson, array, base, base-orphans, base16-bytestring
, base64-bytestring, bytestring, containers, deepseq, ghc-prim
, half, integer-gmp, primitive, QuickCheck, random, scientific
, stdenv, tasty, tasty-hunit, tasty-quickcheck, text, vector
}:
mkDerivation {
  pname = "cborg";
  version = "0.2.4.0";
  sha256 = "34ae38afffa078f8d394325937d0e15431069d4428a7449f6af9d2d16539367f";
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
