{ mkDerivation, base, Cabal, directory, filepath, stdenv }:
mkDerivation {
  pname = "cabal-doctest";
  version = "1.0.7";
  sha256 = "1080ed5f7eca48621853178d701dbee209698eb357957e00a56da6d635a7adec";
  libraryHaskellDepends = [ base Cabal directory filepath ];
  homepage = "https://github.com/phadej/cabal-doctest";
  description = "A Setup.hs helper for doctests running";
  license = stdenv.lib.licenses.bsd3;
}
