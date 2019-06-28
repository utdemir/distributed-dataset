let
pkgs = import ./pkgs.nix;
lib = pkgs.lib;
compilers = [ "ghc865" "ghc844" ];
in 
lib.listToAttrs (builtins.concatMap 
  (c: lib.mapAttrsToList 
        (name: value: { name = "${name}-${c}"; value = value; })
        (import ./default.nix { compiler = c; }))
  compilers
)
