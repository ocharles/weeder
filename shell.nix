let
  hsPkgs = import ./default.nix {};
in
hsPkgs.shellFor {
  withHoogle = true;

  tools = { cabal = "3.2.0.0"; haskell-language-server = "latest"; };

  exactDeps = true;
}
