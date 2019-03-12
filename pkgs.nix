let
nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "c14acca786bc20fcda46b92a447a71b7f11f268d";
  sha256 = "0jkwkic9wp4s2shw8a8990fbxwskwnyhj11ff3vffbla4l3bjqb2";
};
in
import nixpkgs {}
