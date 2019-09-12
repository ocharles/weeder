{ mkDerivation, base, bytestring, containers, HUnit, network
, network-bsd, old-locale, stdenv, time, unix
}:
mkDerivation {
  pname = "hslogger";
  version = "1.3.0.0";
  sha256 = "487f7f2d97b2e2e2e16a6e8675a4e659fda2fd1c352e26f153a964589ac7d6be";
  revision = "1";
  editedCabalFile = "0hvlixqc7vr66qq96flnh3l2p7a6pfmzxf9sn8f243yvsq867yah";
  libraryHaskellDepends = [
    base bytestring containers network network-bsd old-locale time unix
  ];
  testHaskellDepends = [ base HUnit ];
  homepage = "https://github.com/hvr/hslogger/wiki";
  description = "Versatile logging framework";
  license = stdenv.lib.licenses.bsd3;
}
