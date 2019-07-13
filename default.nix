{ pkgs ? import ./pkgs.nix
}:

let
gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

overlays = se: su: {
  "distributed-dataset" =
    se.callCabal2nix
      "distributed-dataset"
      (gitignore ./distributed-dataset)
      {};

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

  # Use newer version
  stratosphere = se.stratosphere_0_40_0;

  # not on Hackage yet
  ormolu =
    se.callCabal2nix
      "ormolu"
      (builtins.fetchGit {
        url = "https://github.com/tweag/ormolu";
        rev = "28c35cc8dffca668e5472bec76a4f489b2b3198e";
      })
      {};
};

haskellPackages = pkgs.haskell.packages.ghc865.override {
  overrides = overlays;
};

in rec
{
  "distributed-dataset" = haskellPackages.distributed-dataset;
  "distributed-dataset-aws" = haskellPackages.distributed-dataset-aws;
  "distributed-dataset-opendatasets" = haskellPackages.distributed-dataset-opendatasets;
  "example-gh" = haskellPackages.example-gh;

  docs = pkgs.runCommand "distributed-dataset-docs" {
    buildInputs =
      [ (haskellPackages.ghcWithPackages (hp: with hp;
          [ distributed-dataset distributed-dataset-aws distributed-dataset-opendatasets ]))
        haskellPackages.standalone-haddock
      ];
  } ''
    mkdir "$out"
    standalone-haddock \
      --dist-dir "$(mktemp -d)" \
      -o "$out" \
      ${distributed-dataset.src} \
      ${distributed-dataset-aws.src} \
      ${distributed-dataset-opendatasets.src}
  '';

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      distributed-dataset
      distributed-dataset-aws
      distributed-dataset-opendatasets
      example-gh
    ];
    buildInputs = with haskellPackages; [
      cabal-install
      ghcid
      ormolu
    ];
    withHoogle = true;
  };
}
