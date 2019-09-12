{ mkDerivation, base, base16-bytestring, bytestring, containers
, cryptohash-sha1, deepseq, directory, extra, fetchgit, file-embed
, filepath, ghc, process, stdenv, temporary, text, time
, transformers, unix-compat, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "hie-bios";
  version = "0.1.1";
  src = fetchgit {
    url = "git://github.com/mpickering/hie-bios";
    sha256 = "1lbhvxx9p8cy2jhgvd9dc7md9g9xjqhck0fwyhz8caa7vcgi53ln";
    rev = "3e4e76064ade245bbe47520bd3eeb7cecdde1070";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base16-bytestring bytestring containers cryptohash-sha1
    deepseq directory extra file-embed filepath ghc process temporary
    text time transformers unix-compat unordered-containers vector yaml
  ];
  executableHaskellDepends = [ base directory filepath ghc ];
  jailbreak = true;
  homepage = "https://github.com/mpickering/hie-bios";
  description = "Set up a GHC API session";
  license = stdenv.lib.licenses.bsd3;
}
