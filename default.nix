{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) stdenv callPackage;
  media    = callPackage ./media.nix { inherit pkgs; };
  terminal = callPackage ./terminal.nix { inherit pkgs; };
in stdenv.mkDerivation {
  name = "dotfiles";
  propagatedBuildInputs = []
    ++ terminal
    ++ media;
}
