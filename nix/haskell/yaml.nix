{ mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, conduit, containers, directory, filepath, hspec, HUnit, libyaml
, mockery, mtl, raw-strings-qq, resourcet, scientific, stdenv
, template-haskell, temporary, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "yaml";
  version = "0.11.1.2";
  sha256 = "0cfedbcf28dfe262671b6ab8d5568c81436f1681c3882d9f9c868a64cff91709";
  configureFlags = [ "-fsystem-libyaml" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring conduit containers directory
    filepath libyaml mtl resourcet scientific template-haskell text
    transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson attoparsec base base-compat bytestring conduit containers
    directory filepath hspec HUnit libyaml mockery mtl raw-strings-qq
    resourcet scientific template-haskell temporary text transformers
    unordered-containers vector
  ];
  homepage = "https://github.com/snoyberg/yaml#readme";
  description = "Support for parsing and rendering YAML documents";
  license = stdenv.lib.licenses.bsd3;
}
