{ mkDerivation, base, base16-bytestring, bytestring, containers
, cryptohash-sha1, deepseq, directory, extra, file-embed, filepath
, ghc, process, stdenv, temporary, text, time, transformers
, unix-compat, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "hie-bios";
  version = "0.2.1";
  sha256 = "3bc3cf9a1996af1b5bc8d3bcf7da298174d57fe4688e16abc5c7318d3364f889";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base16-bytestring bytestring containers cryptohash-sha1
    deepseq directory extra file-embed filepath ghc process temporary
    text time transformers unix-compat unordered-containers vector yaml
  ];
  executableHaskellDepends = [ base directory filepath ghc ];
  homepage = "https://github.com/mpickering/hie-bios";
  description = "Set up a GHC API session";
  license = stdenv.lib.licenses.bsd3;
}
