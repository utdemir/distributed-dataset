let
sources = import ./nix/sources.nix;
in

{ nixpkgs ? sources.nixpkgs
, compiler ? "ghc865"
}:

let
pkgsOrig = import nixpkgs {
  config.allowBroken = true;
  overlays = [
    (se: su: {
      curl = su.curl.override { gssSupport = false; ldapSupport = false; };
    })
  ];
};

pkgsMusl = pkgsOrig.pkgsMusl;

haskell = pkgsMusl.haskell;

gitignore = pkgsMusl.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

fixLocale = pkg: pkgsMusl.lib.overrideDerivation pkg (_: {
  LANG="C.UTF-8";
});

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

  # kernmantle deps
  "kernmantle" =
    se.callCabal2nix
      "kernmantle"
      "${sources.kernmantle}/kernmantle"
      {};
  "vinyl" =
    haskell.lib.dontCheck su.vinyl;

  # Tests don't compile with musl:
  #   hGetContents: invalid argument (invalid byte sequence)
  #   commitBuffer: invalid argument (invalid character)
  "blaze-builder" = fixLocale su.blaze-builder;
  "bsb-http-chunked" = fixLocale su.bsb-http-chunked;
  "code-page" = fixLocale su.code-page;
  "conduit" = fixLocale su.conduit;
  "foundation" = fixLocale su.foundation;
  "hedgehog" = fixLocale su.hedgehog;
  "memory" = fixLocale su.memory;
  "ormolu" = fixLocale su.ormolu;
  "retry" = fixLocale su.retry;
  "shelly" = fixLocale su.shelly;
  "tasty-hedgehog" = fixLocale su.tasty-hedgehog;
  "yaml" = fixLocale su.yaml;

  # Haddock does not work with musl:
  #   haddock: internal error: <stdout>: \
  #     commitBuffer: invalid argument (invalid character)
  #     hGetContents: invalid argument (invalid byte sequence)
  "basement" = fixLocale su.basement;
  "path-io" = fixLocale su.path-io;
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
      haskellPackages.cabal-install
      haskellPackages.ghcid
      haskellPackages.ormolu
      pkgsOrig.niv
    ];
    withHoogle = true;
  };
}
