{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) haskellPackages stdenv termonad-with-packages callPackage;
  inherit (haskellPackages) colour lens pipes;
  extraHaskellPackages = [ colour lens pipes ];
in termonad-with-packages.overrideAttrs(o:  {
  extraHaskellPackages = o.extraHaskellPackages or [] ++ extraHaskellPackages;
})
