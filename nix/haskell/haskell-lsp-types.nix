{ mkDerivation, aeson, base, bytestring, data-default, deepseq
, filepath, hashable, lens, network-uri, scientific, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp-types";
  version = "0.16.0.0";
  sha256 = "0f33bdc2e32495b81a0f1a6074eae1a5ac3cb462d33fce0cbfadade649d99493";
  libraryHaskellDepends = [
    aeson base bytestring data-default deepseq filepath hashable lens
    network-uri scientific text unordered-containers
  ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = stdenv.lib.licenses.mit;
}
