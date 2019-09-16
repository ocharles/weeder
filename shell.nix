with ( import <unstable> {} );

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

                ghcide =
                  haskell.lib.dontCheck super.ghcide;
              }
            );
      };

in
haskellPackages.weeder.env.overrideAttrs
  ( old: { buildInputs = old.buildInputs or [] ++ [ haskellPackages.ghcide ]; } )
