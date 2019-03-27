{ compiler ? "ghc864"
, pkgs ? import ./pkgs.nix
}:

let
gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

overlays = se: su: {
  "distributed-dataset" =
    pkgs.haskell.lib.overrideCabal (
      se.callCabal2nix 
        "distributed-dataset" 
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

  # For some reason this is marked as broken in nixpkgs
  distributed-closure =
    pkgs.haskell.lib.overrideCabal su.distributed-closure (_: { 
      broken = false; 
    });
};

haskellPackages = pkgs.haskell.packages.${compiler}.override {
  overrides = overlays;
};

in
{ 
  "distributed-dataset" = haskellPackages.distributed-dataset;
  "distributed-dataset-aws" = haskellPackages.distributed-dataset-aws;
  "distributed-dataset-opendatasets" = haskellPackages.distributed-dataset-opendatasets;
  "example-gh" = haskellPackages.example-gh;

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
      hlint
    ]; 
    withHoogle = true;
  };
}

