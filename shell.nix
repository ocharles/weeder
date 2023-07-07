let
  hsPkgs = import ./default.nix {};
  pkgs = import (import ./nix/sources.nix).nixpkgs {};
in
hsPkgs.shellFor {
  withHoogle = false;

  # tools = { cabal = "3.2.0.0"; haskell-language-server = "latest"; };

  exactDeps = true;

  buildInputs = [ pkgs.changie ];
}
