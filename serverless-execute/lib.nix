{ pkgs }:

rec {
haskellPackages = pkgs.haskell.packages.ghc841.override {
  overrides = se: su: {
  };
};
hsDrv = haskellPackages.haskellSrc2nix { name = "serverless-execute"; src = ./.; };
library = haskellPackages.callPackage hsDrv {};
devEnv = (pkgs.haskell.lib.overrideCabal library (su: {
  libraryHaskellDepends =
   su.libraryHaskellDepends ++ (with haskellPackages; [
   ]);
})).env;
}
