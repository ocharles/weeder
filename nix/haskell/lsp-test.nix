{ mkDerivation, aeson, aeson-pretty, ansi-terminal, async, base
, bytestring, conduit, conduit-parse, containers, data-default
, Diff, directory, filepath, haskell-lsp, hspec, lens, mtl
, parser-combinators, process, rope-utf16-splay, stdenv, text
, transformers, unix, unordered-containers
}:
mkDerivation {
  pname = "lsp-test";
  version = "0.7.0.0";
  sha256 = "a5b740ff4b75dcfab9f8ff057def1852525973ff17c27cb3e33643b55e4aa2d2";
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal async base bytestring conduit
    conduit-parse containers data-default Diff directory filepath
    haskell-lsp lens mtl parser-combinators process rope-utf16-splay
    text transformers unix unordered-containers
  ];
  testHaskellDepends = [
    aeson base data-default haskell-lsp hspec lens text
    unordered-containers
  ];
  doCheck = false;
  homepage = "https://github.com/bubba/lsp-test#readme";
  description = "Functional test framework for LSP servers";
  license = stdenv.lib.licenses.bsd3;
}
