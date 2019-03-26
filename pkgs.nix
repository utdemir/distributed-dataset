let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "373488e6f4c3dc3bb51cabcb959e4a70eb5d7b2c";
};
in
import nixpkgs {}
