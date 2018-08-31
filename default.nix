{ compiler ? "ghc843"
, pkgs ? import ./pkgs.nix
}:

let
# ghc8.4 support: https://github.com/brendanhay/amazonka/issues/466
amazonkaSrc = pkgs.fetchFromGitHub {
   owner = "brendanhay"; repo = "amazonka";
   rev = "248f7b2a7248222cc21cef6194cd1872ba99ac5d";
   sha256 = "1pvx3v8n4q9jqx2bfvzy20fkwj54w8pwl5hdx8ylnkrzqmn6jnd3";
};

overlays = se: su: {
  "distributed-fork" =
    se.callCabal2nix "distributed-fork" ./distributed-fork {};
  "distributed-fork-aws-lambda" =
    let orig = se.callCabal2nix
                 "distributed-fork-aws-lambda"
                 ./distributed-fork-aws-lambda {};
    in  pkgs.haskell.lib.overrideCabal orig (_: {
          extraLibraries = with pkgs; [
            glibc glibc.static
            (gmp.override { withStatic = true; })
            zlibStatic.static
          ];
          doCheck = false;
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
};

haskellPackages = pkgs.haskell.packages.${compiler}.override {
  overrides = overlays;
};

prepareDev = se: drv:
  pkgs.haskell.lib.addBuildDepends se.${drv} (
    pkgs.lib.optionals pkgs.lib.inNixShell [
      se.cabal-install
    ]
  );

addCompilerName = drv:
  drv.overrideAttrs (old: { name = "${old.name}-${compiler}"; });

output = n: addCompilerName (prepareDev haskellPackages n);

in

{ 
  "distributed-fork" = output "distributed-fork";
  "distributed-fork-aws-lambda" = output "distributed-fork-aws-lambda";
  haskellOverlays = overlays;
}
