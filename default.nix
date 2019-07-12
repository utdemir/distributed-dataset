{ compiler ? "ghc865"
, pkgs ? import ./pkgs.nix
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
  # Haddocks does not work with ghc 8.4
  stratosphere = pkgs.haskell.lib.dontHaddock se.stratosphere_0_40_0;

  # Pulls in a broken dependency on 1.8.1, fixed in master but no new release yet.
  # https://github.com/yesodweb/Shelly.hs/commit/8288d27b93b57574135014d0888cf33f325f7c80
  shelly =
    se.callCabal2nix
      "shelly"
      (builtins.fetchGit {
        url = "https://github.com/yesodweb/Shelly.hs";
        rev = "8288d27b93b57574135014d0888cf33f325f7c80";
      })
      {};

  # Always use the new Cabal
  Cabal = se.Cabal_2_4_1_0;

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

haskellPackages = pkgs.haskell.packages.${compiler}.override {
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
} // (if compiler > "ghc86" then {
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
      stylish-haskell
      ormolu
    ];
    withHoogle = true;
  };
} else {})
