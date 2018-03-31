builtins.map
  (compiler: import ./default.nix { inherit compiler; })
  [ "ghc802" "ghc822" "ghc841" ]
