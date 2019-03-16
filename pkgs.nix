let
nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "f7156588b28ea77c08a07a2d81298ebfb493330e";
  sha256 = "0k2h1vv6izrnwqx838q4833d3r45154nif4828f2hf2jvlx8ah6r";
};
in
import nixpkgs {}
