{ pkgs ? import ../../nixpkgs {} }:

(import ./lib.nix { inherit pkgs; }).library
