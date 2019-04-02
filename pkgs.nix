let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "427320c65921b4a5a7ac5231ba2dedb1e539bac4";
};
in
import nixpkgs {}
