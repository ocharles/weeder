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
              { dhall =
                  self.callPackage ./dhall.nix {};

                weeder =
                  self.callCabal2nix
                    "weeder"
                    ( cleanSource ./. )
                    {};

                cborg =
                  self.callPackage ./cborg.nix {};

                cborg-json =
                  self.callPackage ./cborg-json.nix {};

                prettyprinter =
                  self.callPackage ./prettyprinter.nix {};

                atomic-write =
                  self.callPackage ./atomic-write.nix {};
              }
            );
      };

in
haskellPackages.weeder.env
  # .overrideAttrs
  # ( old: { buildInputs = old.buildInputs or [] ++ [ haskellPackages.ghcide ]; } )
