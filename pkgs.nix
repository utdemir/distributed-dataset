let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "ffc14a60b510be8ebdd970d8647733d96dac3602";
};
in
import nixpkgs {}
