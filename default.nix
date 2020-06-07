let
compiler = "ghc8101";
sources = import ./nix/sources.nix;

pkgsOrig =
  let
    basePkgs = import sources.nixpkgs {};
    patched = basePkgs.applyPatches {
      name = "nixpkgs-patched";
      src = sources.nixpkgs;
      patches = [
        ./nix/patches/0001-Revert-ghc-8.6.3-binary-8.6.5-binary.patch
      ];
    };
  in
    import patched { config.allowBroken = true; };

pkgsMusl = pkgsOrig.pkgsMusl.extend (se: su: {
  fetchgit = pkgsOrig.fetchgit;
});

haskell = pkgsMusl.haskell;
gitignore = pkgsMusl.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

fixLocale = pkg: pkgsMusl.lib.overrideDerivation pkg (_: {
  LANG="C.UTF-8";
});

staticLibs = with pkgsMusl; [
  zlib.static
  (libffi.override { stdenv = makeStaticLibraries stdenv; })
  (gmp.override { withStatic = true; })
];

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
    in  haskell.lib.overrideCabal orig (_: {
          extraLibraries = staticLibs;
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
          extraLibraries = staticLibs;
    });

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

  # standalone-haddock does not compile with cabal3 yet.
  # See: https://github.com/ktvoelker/standalone-haddock/issues/30
  #
  # docs = pkgsMusl.runCommand "distributed-dataset-docs" {
  #   buildInputs =
  #     [ (haskellPackages.ghcWithPackages (hp: with hp;
  #         [ distributed-dataset distributed-dataset-aws distributed-dataset-opendatasets ]))
  #       haskellPackages.standalone-haddock
  #     ];
  # } ''
  #   mkdir "$out"
  #   standalone-haddock \
  #     --dist-dir "$(mktemp -d)" \
  #     -o "$out" \
  #     ${distributed-dataset.src} \
  #     ${distributed-dataset-aws.src} \
  #     ${distributed-dataset-opendatasets.src}
  # '';

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
    # shellFor unsets LOCALE_ARCHIVE when GHC is compiled with musl, which breaks
    # glibc-linked programs in nix-shell in weird ways.
    shellHook = ''
      export LOCALE_ARCHIVE="${pkgsOrig.glibcLocales}/lib/locale/locale-archive"
    '';
  };
}
