{ mkDerivation, base, bytestring, containers, deepseq, HUnit
, network, network-bsd, old-locale, stdenv, time, unix
}:
mkDerivation {
  pname = "hslogger";
  version = "1.3.1.0";
  sha256 = "7f2364f6c0b9c5b85a257267a335816126ef2471c817a42797a5d3c57acaca5b";
  libraryHaskellDepends = [
    base bytestring containers deepseq network network-bsd old-locale
    time unix
  ];
  testHaskellDepends = [ base HUnit ];
  homepage = "https://github.com/hvr/hslogger/wiki";
  description = "Versatile logging framework";
  license = stdenv.lib.licenses.bsd3;
}
