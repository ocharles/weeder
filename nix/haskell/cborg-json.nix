{ mkDerivation, aeson, aeson-pretty, base, bytestring, cborg
, criterion, deepseq, directory, process, scientific, stdenv, text
, unordered-containers, vector, zlib
}:
mkDerivation {
  pname = "cborg-json";
  version = "0.2.2.0";
  sha256 = "ab68a2457cb71a76699d7a8df07a880ea70c51d2c1a891b12669ca9ccfa7517b";
  revision = "2";
  editedCabalFile = "1hbabjvmyqha75v2ivyvj6yzrnj9vs3h9988j4p68x9bcwmgyjyd";
  libraryHaskellDepends = [
    aeson aeson-pretty base cborg scientific text unordered-containers
    vector
  ];
  benchmarkHaskellDepends = [
    aeson base bytestring cborg criterion deepseq directory process
    zlib
  ];
  doCheck = false;
  homepage = "https://github.com/well-typed/cborg";
  description = "A library for encoding JSON as CBOR";
  license = stdenv.lib.licenses.bsd3;
}
