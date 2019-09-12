{ mkDerivation, aeson, async, base, binary, bytestring, containers
, data-default, deepseq, directory, extra, fetchgit, filepath, ghc
, ghc-boot, ghc-boot-th, ghc-paths, hashable, haskell-lsp
, haskell-lsp-types, hie-bios, lens, lsp-test, mtl, network-uri
, optparse-applicative, parser-combinators, prettyprinter
, prettyprinter-ansi-terminal, rope-utf16-splay, safe-exceptions
, shake, sorted-list, stdenv, stm, syb, tasty, tasty-hunit, text
, time, transformers, unix, unordered-containers, utf8-string
}:
mkDerivation {
  pname = "ghcide";
  version = "0.0.2";
  src = fetchgit {
    url = "git://github.com/ocharles/ghcide";
    sha256 = "1hpib9n5lnkixvk4h5vp5lx77gczwpqv61i0v9g0bjjjg1vg0s5r";
    rev = "973c3e366328d6f95534638882ca3dfe9e4aa2fa";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base binary bytestring containers data-default deepseq
    directory extra filepath ghc ghc-boot ghc-boot-th hashable
    haskell-lsp haskell-lsp-types mtl network-uri prettyprinter
    prettyprinter-ansi-terminal rope-utf16-splay safe-exceptions shake
    sorted-list stm syb text time transformers unix
    unordered-containers utf8-string
  ];
  executableHaskellDepends = [
    base containers data-default directory extra filepath ghc ghc-paths
    haskell-lsp hie-bios optparse-applicative shake text
  ];
  testHaskellDepends = [
    base containers extra filepath haskell-lsp-types lens lsp-test
    parser-combinators tasty tasty-hunit text
  ];
  homepage = "https://github.com/digital-asset/ghcide#readme";
  description = "The core of an IDE";
  license = stdenv.lib.licenses.asl20;
}
