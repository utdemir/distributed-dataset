let
nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "4c58c20efccc663929e70d138f49cdad0e7537e7";
  sha256 = "0ynd9bg5n5c2fsvbv57dj6hwwv9pcibzpnlg3dsbxd1v8p3kv1qz";
};
in
import nixpkgs { config.allowUnfree = true; }
