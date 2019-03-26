let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "f7156588b28ea77c08a07a2d81298ebfb493330e";
};
in
import nixpkgs {}
