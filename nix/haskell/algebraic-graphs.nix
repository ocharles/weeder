{ mkDerivation, array, base, containers, deepseq, extra
, inspection-testing, mtl, QuickCheck, stdenv, transformers
}:
mkDerivation {
  pname = "algebraic-graphs";
  version = "0.5";
  sha256 = "89b9fecf8245476ec823355125fcb95decf41fd9784e807d7bd0d09f0a79c50b";
  libraryHaskellDepends = [
    array base containers deepseq mtl transformers
  ];
  testHaskellDepends = [
    array base containers deepseq extra inspection-testing mtl
    QuickCheck transformers
  ];
  homepage = "https://github.com/snowleopard/alga";
  description = "A library for algebraic graph construction and transformation";
  license = stdenv.lib.licenses.mit;
}
