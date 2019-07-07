let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "dc0dbaf0bd8ad9a04011ee734709b4f3e2ce15f0";
};
in
import nixpkgs { config = { allowBroken = true; }; }
