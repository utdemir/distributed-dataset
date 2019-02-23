with (import ./pkgs.nix).lib;
concatLists [
  (attrValues (import ./default.nix {}))
  (attrValues (import ./default.nix { compiler = "ghc844"; }))
]

