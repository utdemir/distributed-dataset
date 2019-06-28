let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "a2075a2c314ccb3df6522dd957bb4e237446dc49";
};
in
import nixpkgs { config = { allowBroken = true; }; }
