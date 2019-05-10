let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "1da6717d567314921f6c53dd8b979b1d1b4fe956";
};
in
import nixpkgs { config = { allowBroken = true; }; }
