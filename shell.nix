let
  hsPkgs = import ./default.nix {};
in
hsPkgs.shellFor {
  withHoogle = false;

  # tools = { cabal = "3.2.0.0"; haskell-language-server = "latest"; };
  # tools = { hiedb = "latest"; };

  exactDeps = false;
}
