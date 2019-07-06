{ compiler ? "ghc865"
, pkgs ? import ./pkgs.nix
}:

let
gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

subdirectory = subdir: drv: pkgs.runCommand "subdirectory" {} ''
    cp -r "${drv}/${subdir}" "$out"
'';

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
  stratosphere = pkgs.haskell.lib.dontHaddock se.stratosphere_0_39_0;

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
  
  # hie
  hie-bios =
    se.callCabal2nix 
      "hie-bios"
      (builtins.fetchGit {
        url = "https://github.com/mpickering/hie-bios";
        rev = "8427e424a83c2f3d60bdd26c02478c00d2189a73";
      })
      {};
      
  hie-core =
    pkgs.haskell.lib.dontHaddock(
      se.callCabal2nix 
        "hie-core"
        (subdirectory "compiler/hie-core" (builtins.fetchGit {
          url = "https://github.com/digital-asset/daml";
          rev = "b748fab0f9fd0a4e6294e047004c9527bf26e007";
        }))
        {}
    );

  haskell-lsp = 
    se.callCabal2nix 
      "haskell-lsp"
      (builtins.fetchGit {
        url = "https://github.com/alanz/haskell-lsp";
        rev = "d00f83700cdc681f65015c7343206c134919d15f";
      })
      {};
      
  haskell-lsp-types = 
    se.callCabal2nix 
      "haskell-lsp"
      (subdirectory "haskell-lsp-types" (builtins.fetchGit {
        url = "https://github.com/alanz/haskell-lsp";
        rev = "d00f83700cdc681f65015c7343206c134919d15f";
      }))
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
      hie-core
    ]; 
    withHoogle = true;
  };
  
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
}

