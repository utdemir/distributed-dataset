let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "005fb92d32c9d5b636846ce30ae01aab81454161";
};
in
import nixpkgs {}
