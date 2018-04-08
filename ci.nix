{ pkgs ? null }:

builtins.map
  (compiler: import ./default.nix { inherit compiler pkgs; })
  [ "ghc802" "ghc822" "ghc841" ]
