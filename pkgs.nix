let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "5f39726ce4abdc7654f652a2443ba32b5ad1d120";
};
in
import nixpkgs {}
