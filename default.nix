{ compiler-nix-name ? "ghc884" }:
let 
  haskellNix = import (import ./nix/sources.nix)."haskell.nix" {};

  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;

  nixpkgsArgs = haskellNix.nixpkgsArgs;

  pkgs = import nixpkgsSrc nixpkgsArgs;

in 
pkgs.haskell-nix.project {
  inherit compiler-nix-name;

  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "weeder";
    src = ./.;
  };
}
