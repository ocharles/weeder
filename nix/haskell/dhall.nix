{ mkDerivation, aeson, aeson-pretty, ansi-terminal, atomic-write
, base, bytestring, case-insensitive, cborg, cborg-json, containers
, contravariant, cryptonite, data-fix, deepseq, Diff, directory
, doctest, dotgen, either, exceptions, filepath, foldl, gauge
, generic-random, hashable, haskeline, http-client, http-client-tls
, http-types, lens-family-core, megaparsec, memory, mockery, mtl
, network-uri, optparse-applicative, parser-combinators, parsers
, pretty-simple, prettyprinter, prettyprinter-ansi-terminal
, profunctors, QuickCheck, quickcheck-instances, repline
, scientific, semigroups, serialise, special-values, spoon, stdenv
, tasty, tasty-expected-failure, tasty-hunit, tasty-quickcheck
, template-haskell, text, th-lift-instances, transformers
, transformers-compat, turtle, unordered-containers, uri-encode
, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.30.0";
  sha256 = "f2be9599ddd88602c1577b0ca57849c9827c9e700e105102cecc17c56b7c4a81";
  revision = "1";
  editedCabalFile = "1pazhb3h1rabb80wxh29k5yfp915zqp1gmhcv4mx7ibzv9zw7miq";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal atomic-write base bytestring
    case-insensitive cborg cborg-json containers contravariant
    cryptonite data-fix deepseq Diff directory dotgen either exceptions
    filepath hashable haskeline http-client http-client-tls http-types
    lens-family-core megaparsec memory mtl network-uri
    optparse-applicative parser-combinators parsers pretty-simple
    prettyprinter prettyprinter-ansi-terminal profunctors repline
    scientific serialise template-haskell text th-lift-instances
    transformers transformers-compat unordered-containers uri-encode
    vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring cborg containers data-fix deepseq directory doctest
    either filepath foldl generic-random lens-family-core megaparsec
    mockery prettyprinter QuickCheck quickcheck-instances scientific
    semigroups serialise special-values spoon tasty
    tasty-expected-failure tasty-hunit tasty-quickcheck text
    transformers turtle unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers directory gauge serialise text
  ];
  doCheck = false;
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
