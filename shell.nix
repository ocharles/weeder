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

                lsp-test =
                  haskell.lib.disableLibraryProfiling super.lsp-test;

                haskell-lsp =
                  haskell.lib.disableLibraryProfiling super.haskell-lsp;

                haskell-lsp-types =
                  haskell.lib.disableLibraryProfiling super.haskell-lsp-types;

                ghcide =
                  haskell.lib.dontCheck ( haskell.lib.disableLibraryProfiling super.ghcide );

                lens =
                  haskell.lib.disableLibraryProfiling super.lens;
              }
            );
      };

in
haskellPackages.weeder.env.overrideAttrs
  ( old: { buildInputs = old.buildInputs or [] ++ [ haskellPackages.ghcide ]; } )
