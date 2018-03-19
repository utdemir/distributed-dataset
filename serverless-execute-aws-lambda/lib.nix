{ pkgs }:

rec {
haskellPackages = pkgs.haskell.packages.ghc822.override {
  overrides = se: su: {
    serverless-execute =
      se.callPackage (import ../serverless-execute/lib.nix { inherit pkgs; }).hsDrv {};
  };
};
library_ = haskellPackages.callCabal2nix "serverless-execute-aws-lambda" ./. {};
library = pkgs.haskell.lib.overrideCabal library_ (drv: {
  extraLibraries = [
    pkgs.glibc pkgs.glibc.static
    (pkgs.gmp.override { withStatic = true; })
    pkgs.zlibStatic.static
  ];
  testSystemDepends = [ pkgs.file ];
});
devEnv = (pkgs.haskell.lib.overrideCabal library (su: {
  libraryHaskellDepends =
   su.libraryHaskellDepends ++ (with haskellPackages; [
     cabal-install
   ]);
})).env;
}
