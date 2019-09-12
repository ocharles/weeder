{ mkDerivation, ansi-wl-pprint, base, base-compat, bytestring
, containers, criterion, deepseq, doctest, fetchgit, mtl
, pgp-wordlist, QuickCheck, random, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, transformers
}:
mkDerivation {
  pname = "prettyprinter";
  version = "1.3.0";
  src = fetchgit {
    url = "git://github.com/quchen/prettyprinter";
    sha256 = "17hcy479ivgdl9crmyh9lcaibrgxkc7fzbjv8x30bk1kgz44jsfa";
    rev = "4ab50e36b0a0f290cc2795a69ac9b159a62b5fe1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/prettyprinter; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [
    base bytestring doctest pgp-wordlist QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base base-compat containers criterion deepseq mtl
    QuickCheck random text transformers
  ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "A modern, easy to use, well-documented, extensible pretty-printer";
  license = stdenv.lib.licenses.bsd2;
}
