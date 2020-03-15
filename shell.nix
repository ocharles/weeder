with ( import <nixpkgs> {} );

let
  inherit ( lib ) cleanSource composeExtensions;

  haskellPackages =
    haskell.packages.ghc881.override
      { overrides =
          composeExtensions
            ( haskell.lib.packagesFromDirectory
                { directory =
                    ./nix/haskell;
                }
            )
            ( self:
              super:
              { weeder =
                  self.callCabal2nix
                    "weeder"
                    ( cleanSource ./. )
                    {};
              }
            );
      };

in
haskellPackages.weeder.env
