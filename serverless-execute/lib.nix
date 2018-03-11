{ pkgs }:

rec {
haskellPackages = pkgs.haskell.packages.ghc841.override {
  overrides = se: su: {
  };
};
library = haskellPackages.callCabal2nix "serverless-execute" ./. {};
devEnv = (pkgs.haskell.lib.overrideCabal library (su: {
  libraryHaskellDepends =
   su.libraryHaskellDepends ++ (with haskellPackages; [
   ]);
})).env;
}
