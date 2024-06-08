{
  description = "weeder";

  nixConfig = {
    extra-substituters = [
      "https://weeder.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "weeder.cachix.org-1:RdUmJalaHD50fGg3bNqsOGmGzRRqTIacmPBu/Xtjl3I="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    haskellNix.url = github:input-output-hk/haskell.nix;
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };

        weeder = pkgs.haskell-nix.project {
          compiler-nix-name = "ghc981";

          cabalProjectLocal = builtins.readFile ./cabal.project.haskell-nix;

          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "weeder";
            src = ./.;
          }; 

          modules = [
            {
              reinstallableLibGhc = false;
              enableLibraryProfiling = true;
              nonReinstallablePkgs = [
                "rts" "ghc-prim" "integer-gmp" "integer-simple" "base"
                "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
                "ghc-bignum" "system-cxx-std-lib" "ghc" "binary" "bytestring" "containers" 
                "directory" "exceptions" "filepath" "hpc" "process" "semaphore-compat" "stm" 
                "time" "transformers" "unix" "mtl"
              ];
            }
          ];
        };
      in
      {
        packages.default = weeder.hsPkgs.weeder.components.library;

        devShells.default = weeder.shellFor {
          withHoogle = false;
          tools = { cabal = "latest"; };
          exactDeps = false;
          buildInputs = [ pkgs.changie ];
        };
      }
    );
}
