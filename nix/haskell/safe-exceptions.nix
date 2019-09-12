{ mkDerivation, base, deepseq, exceptions, hspec, stdenv
, transformers, void
}:
mkDerivation {
  pname = "safe-exceptions";
  version = "0.1.7.0";
  sha256 = "18cddc587b52b6faa0287fb6ad6c964d1562571ea2c8ff57a194dd54b5fba069";
  revision = "5";
  editedCabalFile = "0yghh99yg24pzhzrflvgi4ip56ln7a56871pl3q70sm8rszy8vbr";
  libraryHaskellDepends = [ base deepseq exceptions transformers ];
  testHaskellDepends = [ base hspec void ];
  homepage = "https://github.com/fpco/safe-exceptions#readme";
  description = "Safe, consistent, and easy exception handling";
  license = stdenv.lib.licenses.mit;
}
