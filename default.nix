{ compiler-nix-name ? "ghc922" }:
let
  haskellNix = import (import ./nix/sources.nix)."haskell.nix" {};

  nixpkgsSrc = haskellNix.sources.nixpkgs;

  nixpkgsArgs = haskellNix.nixpkgsArgs;

  pkgs = import nixpkgsSrc nixpkgsArgs;

in
pkgs.haskell-nix.project {
  inherit compiler-nix-name;

  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "weeder";
    src = ./.;
  };

  modules = [(pkgs.lib.optionalAttrs (compiler-nix-name == "ghc901") {
    nonReinstallablePkgs = [
      "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th"
      "ghc-bignum" "exceptions" "stm"
      "ghc-boot"
      "ghc" "Cabal" "Win32" "array" "binary" "bytestring" "containers"
      "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
      # "ghci" "haskeline"
      "hpc"
      "mtl" "parsec" "process" "text" "time" "transformers"
      "unix" "xhtml" "terminfo"
    ];
  })];
}
