let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "7a253894aecbb12ba3a72934a14a6d81400d35e8";
};
in
import nixpkgs {}
