{ compiler ? "ghc822"
, pkgs ? null
}:

let
pkgs' = if pkgs == null
          then import ((import <nixpkgs> {}).fetchFromGitHub {
            owner = "NixOS"; repo = "nixpkgs";
            rev = "922fcbb310c25adb66530316f6851df7142314f0";
            sha256 = "0njzvl7j9cbgffkknz0ps9i8rw70pjxx56h6cl3lzhqfqc67b5mw";
          }) {}
        else pkgs;

# conduit-1.3 support: https://github.com/brendanhay/amazonka/pull/454
amazonkaSrc = pkgs'.fetchFromGitHub {
  owner = "brendanhay"; repo = "amazonka";
  rev = "97273545cf37672bec0cdcf78a9d68c274bbb6c2";
  sha256 = "02kp531j5a1046yx4lr01df93r4r0a4a7h6ak9ky91d5iyqg6ih6";
};

haskellPackages = pkgs'.haskell.packages.${compiler}.override {
  overrides = se: su: {
    serverless-execute =
      se.callCabal2nix "serverless-execute" ./serverless-execute {};
    serverless-execute-aws-lambda =
      let orig = se.callCabal2nix
                    "serverless-execute-aws-lambda"
                    ./serverless-execute-aws-lambda {};
      in  pkgs'.haskell.lib.overrideCabal orig (_: {
            extraLibraries = [
              pkgs'.glibc pkgs'.glibc.static
              (pkgs'.gmp.override { withStatic = true; })
              pkgs'.zlibStatic.static
            ];
            testSystemDepends = [ pkgs'.file ];
      });
    serverless-batch =
      se.callCabal2nix "serverless-batch" ./serverless-batch {};

    # Overrides

    concurrent-output =
      pkgs'.haskell.lib.doJailbreak su.concurrent-output;

    #amazonka-core =
    #  pkgs'.haskell.lib.overrideCabal su.amazonka-core (_: {
    #    src = amazonkaSrc;
    #    patchPhase = ''
    #      TMPDIR=$(mktemp -d)
    #      mv * $TMPDIR/; mv $TMPDIR/core/* . #*/
    #    '';
    #    jailbreak = true;
    #  });
    #
    #amazonka =
    #  pkgs'.haskell.lib.overrideCabal su.amazonka (_: {
    #    src = amazonkaSrc;
    #    patchPhase = ''
    #      TMPDIR=$(mktemp -d)
    #      mv * $TMPDIR/; mv $TMPDIR/amazonka/* . #*/
    #    '';
    #    jailbreak = true;
    #  });
    #
    #amazonka-test =
    #  pkgs'.haskell.lib.overrideCabal su.amazonka-test (_: {
    #    src = amazonkaSrc;
    #    patchPhase = ''
    #      TMPDIR=$(mktemp -d)
    #      mv * $TMPDIR/; mv $TMPDIR/test/* . #*/
    #    '';
    #    jailbreak = true;
    #  });
  };
};

prepareDev = se: drv:
  pkgs'.haskell.lib.addBuildDepends se.${drv} (
    pkgs'.lib.optionals pkgs'.lib.inNixShell [
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
