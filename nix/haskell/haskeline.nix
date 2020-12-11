{ mkDerivation, base, bytestring, containers, directory, exceptions
, filepath, HUnit, process, stdenv, stm, terminfo, text
, transformers, unix
}:
mkDerivation {
  pname = "haskeline";
  version = "0.8.1.0";
  sha256 = "bc9be8f7b5232e53f6f863828f1649a92a585ea1be254877c118cc42729fda64";
  configureFlags = [ "-fterminfo" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory exceptions filepath process
    stm terminfo transformers unix
  ];
  executableHaskellDepends = [ base containers ];
  testHaskellDepends = [
    base bytestring containers HUnit process text unix
  ];
  doCheck = false;
  homepage = "https://github.com/judah/haskeline";
  description = "A command-line interface for user input, written in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
