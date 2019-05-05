let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "da6ecbdbc851f057035563bbeed4bf43dead7b5a";
};
in
import nixpkgs { config = { allowBroken = true; }; }
