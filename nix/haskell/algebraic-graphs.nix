{ mkDerivation, array, base, base-compat, base-orphans, containers
, deepseq, extra, inspection-testing, mtl, QuickCheck, stdenv
}:
mkDerivation {
  pname = "algebraic-graphs";
  version = "0.4";
  sha256 = "c905d32a6178a11e3c8096dbbf3bd19e570e87362c51fdc8653b43a51e46d3b7";
  libraryHaskellDepends = [
    array base base-compat containers deepseq mtl
  ];
  testHaskellDepends = [
    array base base-compat base-orphans containers extra
    inspection-testing QuickCheck
  ];
  doCheck = false;
  homepage = "https://github.com/snowleopard/alga";
  description = "A library for algebraic graph construction and transformation";
  license = stdenv.lib.licenses.mit;
}
