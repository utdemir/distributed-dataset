{ pkgs }:

rec {
haskellPackages = pkgs.haskell.packages.ghc822.override {
  overrides = se: su: {
    serverless-execute =
      se.callPackage (import ../serverless-execute/lib.nix { inherit pkgs; }).hsDrv {};
  };
};
library = haskellPackages.callCabal2nix "serverless-execute-aws-lambda" ./. {};
devEnv = (pkgs.haskell.lib.overrideCabal library (su: {
  libraryHaskellDepends =
   su.libraryHaskellDepends ++ (with haskellPackages; [
   ]);
})).env;
}
