let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "f38fbe0d4949209d1dee9194d14718defcf9aece";
};
in
import nixpkgs { config = { allowBroken = true; }; }
