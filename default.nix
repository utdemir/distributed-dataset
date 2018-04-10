{ compiler ? "ghc822"
, pkgs ? null
}:

let
pkgs' = if pkgs == null
          then import ((import <nixpkgs> {}).fetchFromGitHub {
            owner = "NixOS"; repo = "nixpkgs";
            rev = "040a9ab240fba0b0dae5b48692fff7be50d3281c";
            sha256 = "0ganah2c7a4ankfxzc1lxn3bmfkk1g2sqrsab4j9pj5qzmjajq68";
          }) {}
        else pkgs;

# ghc8.4 support: https://github.com/brendanhay/amazonka/pull/456
# amazonkaSrc = pkgs'.fetchFromGitHub {
#   owner = "brendanhay"; repo = "amazonka";
#   rev = "78b2736448f81e09b712b8d01cef3d1f45fb02ac";
#   sha256 = "1217cf3562i029lj47az7m1qsk7fn9rrjjqhknl4gf5l21s4h0fa";
# };
# conduit-1.3 support: https://github.com/brendanhay/amazonka/pull/454
amazonkaSrc = pkgs'.fetchFromGitHub {
   owner = "brendanhay"; repo = "amazonka";
   rev = "97273545cf37672bec0cdcf78a9d68c274bbb6c2";
   sha256 = "02kp531j5a1046yx4lr01df93r4r0a4a7h6ak9ky91d5iyqg6ih6";
};
# amazonkaSrc = ../amazonka;

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

    # GHC 8.4 support, it's on master but not released
    ini =
      pkgs'.haskell.lib.overrideCabal su.ini (_: {
        src = pkgs'.fetchFromGitHub {
          owner = "chrisdone"; repo = "ini";
          rev = "477e0ffe76b43fab9dd9ff1ad4925b9047683eb0";
          sha256 = "06yb1vjpay228nbps37dcl4mjyg74c4x4h7bgvrnyq0adjib5win";
        };
      });

    amazonka-core =
      pkgs'.haskell.lib.overrideCabal su.amazonka-core (_: {
        src = amazonkaSrc;
        patchPhase = ''
          TMPDIR=$(mktemp -d)
          mv * $TMPDIR/; mv $TMPDIR/core/* . #*/
        '';
        jailbreak = true;
      });

    amazonka =
      pkgs'.haskell.lib.overrideCabal su.amazonka (_: {
        src = amazonkaSrc;
        patchPhase = ''
          TMPDIR=$(mktemp -d)
          mv * $TMPDIR/; mv $TMPDIR/amazonka/* . #*/
        '';
        jailbreak = true;
      });

    amazonka-test =
      pkgs'.haskell.lib.overrideCabal su.amazonka-test (_: {
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
  pkgs'.haskell.lib.addBuildDepends se.${drv} (
    pkgs'.lib.optionals pkgs'.lib.inNixShell [
      se.stylish-haskell se.cabal-install
    ]
  );

addCompilerName = drv:
  drv.overrideAttrs (old: { name = "${old.name}-${compiler}"; });

output = n: addCompilerName (prepareDev haskellPackages n);

in

{
  serverless-execute = output "serverless-execute";
  serverless-execute-aws-lambda = output "serverless-execute-aws-lambda";
  serverless-batch = output "serverless-batch";
}
