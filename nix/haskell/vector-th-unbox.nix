{ mkDerivation, base, data-default, fetchgit, stdenv
, template-haskell, vector
}:
mkDerivation {
  pname = "vector-th-unbox";
  version = "0.2.1.7";
  src = fetchgit {
    url = "git://github.com/phadej/vector-th-unbox";
    sha256 = "1kwgh0bn1pf6bh0liwbgqb68mihlax5malx9zh6ids7qhq22nh3p";
    rev = "e1e81c954f187c3d2a752253627a97e1d7a3b82c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base template-haskell vector ];
  testHaskellDepends = [ base data-default vector ];
  description = "Deriver for Data.Vector.Unboxed using Template Haskell";
  license = stdenv.lib.licenses.bsd3;
}
