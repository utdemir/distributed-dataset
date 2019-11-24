let
sources = import ./nix/sources.nix;
in

{ pkgsOrig ? import sources.nixpkgs { config.allowBroken = true; }
, compiler ? "ghc865"
}:

let
pkgsMusl = pkgsOrig.pkgsMusl;
haskell = pkgsMusl.haskell;

gitignore = pkgsMusl.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

overlays = se: su: {
  # `cabal2nix` from `pkgsOrig` instead of `pkgsMusl`.
  callCabal2nix = name: src:
    let nix = pkgsOrig.haskellPackages.haskellSrc2nix { inherit name src; };
    in  se.callPackage "${nix}/default.nix";

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
    in  haskell.lib.overrideCabal orig (_: {
          extraLibraries = with pkgsMusl; [
            zlib.static
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
    in  haskell.lib.overrideCabal orig (_: {
          extraLibraries = with pkgsMusl; [
            musl zlib.static
            (libffi.override { stdenv = makeStaticLibraries stdenv; })
            (gmp.override { withStatic = true; })
          ];
    });

  # Tests don't compile with musl:
  #   hGetContents: invalid argument (invalid byte sequence)
  #   commitBuffer: invalid argument (invalid character)
  # Most (all?) of them are property-based tests, and musl
  # does not handle some non-standard UTF-8 encodings; so
  # they probably all share the same underlying issue.
  "blaze-builder" = haskell.lib.dontCheck su.blaze-builder;
  "bsb-http-chunked" = haskell.lib.dontCheck su.bsb-http-chunked;
  "code-page" = haskell.lib.dontCheck su.code-page;
  "conduit" = haskell.lib.dontCheck su.conduit;
  "foundation" = haskell.lib.dontCheck su.foundation;
  "hedgehog" = haskell.lib.dontCheck su.hedgehog;
  "memory" = haskell.lib.dontCheck su.memory;
  "retry" = haskell.lib.dontCheck su.retry;
  "shelly" = haskell.lib.dontCheck su.shelly;
  "tasty-hedgehog" = haskell.lib.dontCheck su.tasty-hedgehog;
  "yaml" = haskell.lib.dontCheck su.yaml;

  # Haddock does not work with musl:
  #   haddock: internal error: <stdout>: \
  #     commitBuffer: invalid argument (invalid character)
  #     hGetContents: invalid argument (invalid byte sequence)
  "basement" = haskell.lib.dontHaddock su.basement;
  "path-io" = haskell.lib.dontHaddock su.path-io;
};

haskellPackages = haskell.packages.${compiler}.override {
  overrides = overlays;
};

in rec
{
  "distributed-dataset" = haskellPackages.distributed-dataset;
  "distributed-dataset-aws" = haskellPackages.distributed-dataset-aws;
  "distributed-dataset-opendatasets" = haskellPackages.distributed-dataset-opendatasets;
  "example-gh" = haskellPackages.example-gh;

  inherit haskellPackages;

  docs = pkgsMusl.runCommand "distributed-dataset-docs" {
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
      pkgsOrig.haskellPackages.cabal-install
      pkgsOrig.haskellPackages.ghcid
      pkgsOrig.haskellPackages.ormolu
      pkgsOrig.niv
    ];
    withHoogle = true;
  };
}
