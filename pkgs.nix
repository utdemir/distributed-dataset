let
nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "6c5097ba02bd0d39bc2d24dbd18ac74ad8c93645";
  sha256 = "0dg89ngysl74m71cal7aidpmflcm4mx17g99w6bkcyl7lmy2i1jn";
};
in
import nixpkgs {}
