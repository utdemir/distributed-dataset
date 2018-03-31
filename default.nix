{ compiler ? "ghc822" }:

let

pkgs = import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "040a9ab240fba0b0dae5b48692fff7be50d3281c";
  sha256 = "0ganah2c7a4ankfxzc1lxn3bmfkk1g2sqrsab4j9pj5qzmjajq68";
}) {};

# conduit-1.3 support: https://github.com/brendanhay/amazonka/pull/454
amazonkaSrc = pkgs.fetchFromGitHub {
  owner = "brendanhay"; repo = "amazonka";
  rev = "97273545cf37672bec0cdcf78a9d68c274bbb6c2";
  sha256 = "02kp531j5a1046yx4lr01df93r4r0a4a7h6ak9ky91d5iyqg6ih6";
};

haskellPackages = pkgs.haskell.packages.${compiler}.override {
  overrides = se: su: {
    serverless-execute =
      se.callCabal2nix "serverless-execute" ./serverless-execute {};
    serverless-execute-aws-lambda =
      let orig = se.callCabal2nix
                    "serverless-execute-aws-lambda"
                    ./serverless-execute-aws-lambda {};
      in  pkgs.haskell.lib.overrideCabal orig (_: {
            extraLibraries = [
              pkgs.glibc pkgs.glibc.static
              (pkgs.gmp.override { withStatic = true; })
              pkgs.zlibStatic.static
            ];
            testSystemDepends = [ pkgs.file ];
      });
    serverless-batch =
      se.callCabal2nix "serverless-batch" ./serverless-batch {};

    # Overrides

    concurrent-output =
      pkgs.haskell.lib.doJailbreak su.concurrent-output;

    amazonka-core =
      pkgs.haskell.lib.overrideCabal su.amazonka-core (_: {
        src = amazonkaSrc;
        patchPhase = ''
          TMPDIR=$(mktemp -d)
          mv * $TMPDIR/; mv $TMPDIR/core/* . #*/
        '';
        jailbreak = true;
      });

    amazonka =
      pkgs.haskell.lib.overrideCabal su.amazonka (_: {
        src = amazonkaSrc;
        patchPhase = ''
          TMPDIR=$(mktemp -d)
          mv * $TMPDIR/; mv $TMPDIR/amazonka/* . #*/
        '';
        jailbreak = true;
      });

    amazonka-test =
      pkgs.haskell.lib.overrideCabal su.amazonka-test (_: {
        src = amazonkaSrc;
        patchPhase = ''
          TMPDIR=$(mktemp -d)
          mv * $TMPDIR/; mv $TMPDIR/test/* . #*/
        '';
        jailbreak = true;
      });
  };
};

prepareDev = se: drv:
  pkgs.haskell.lib.addBuildDepends se.${drv} (
    pkgs.lib.optionals pkgs.lib.inNixShell [
      se.stylish-haskell se.cabal-install
    ]
  );

in

{
  serverless-execute =
    prepareDev haskellPackages "serverless-execute";
  serverless-execute-aws-lambda =
    prepareDev haskellPackages "serverless-execute-aws-lambda";
  serverless-batch =
    prepareDev haskellPackages "serverless-batch";
}
