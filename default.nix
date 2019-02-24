{ compiler ? "ghc863"
, pkgs ? import ./pkgs.nix
}:

let
gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

overlays = se: su: {
  "distributed-dataset" =
    pkgs.haskell.lib.overrideCabal (
      se.callCabal2nix 
        "distributed-dataset-foo" 
        (gitignore ./distributed-dataset) 
        {}
    ) (_: {
      doHaddock = compiler > "ghc86";
    });
  "distributed-dataset-aws" =
    let orig = se.callCabal2nix
                 "distributed-dataset-aws"
                 (gitignore ./distributed-dataset-aws)
                 {};
    in  pkgs.haskell.lib.overrideCabal orig (_: {
          extraLibraries = with pkgs; [
            glibc glibc.static zlib.static
            (libffi.override { stdenv = makeStaticLibraries stdenv; })
            (gmp.override { withStatic = true; })
          ];
        doCheck = false;
    });
  "distributed-dataset-opendatasets" =
    se.callCabal2nix 
      "distributed-dataset-opendatasets" 
      (gitignore ./distributed-dataset-opendatasets)
      {};

  "example-gh" =
    let orig = se.callCabal2nix
                 "example-gh"
                 (gitignore ./examples/gh)
                 {};
    in  pkgs.haskell.lib.overrideCabal orig (_: {
          extraLibraries = with pkgs; [
            glibc glibc.static zlib.static 
            (libffi.override { stdenv = makeStaticLibraries stdenv; })
            (gmp.override { withStatic = true; })
          ];
    });

  # tests fail on ghc8.6
  distributed-closure =
    pkgs.haskell.lib.dontCheck su.distributed-closure ;
};

haskellPackages = pkgs.haskell.packages.${compiler}.override {
  overrides = overlays;
};

prepareDev = se: drv:
  pkgs.haskell.lib.addBuildDepends se.${drv} (
    pkgs.lib.optionals pkgs.lib.inNixShell [
      se.cabal-install se.ghcid se.stylish-haskell
    ]
  );

addCompilerName = drv:
  drv.overrideAttrs (old: { name = "${old.name}-${compiler}"; });

output = n: addCompilerName (prepareDev haskellPackages n);

in
{ 
  "distributed-dataset" = output "distributed-dataset";
  "distributed-dataset-aws" = output "distributed-dataset-aws";
  "distributed-dataset-opendatasets" = output "distributed-dataset-opendatasets";
  "example-gh" = output "example-gh";
}
