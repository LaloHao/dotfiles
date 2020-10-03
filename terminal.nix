{ pkgs }:
let
  inherit (pkgs) haskellPackages termonad-with-packages callPackage;
  inherit (haskellPackages) colour lens pipes;
  extraHaskellPackages = [ colour lens pipes ];
in let
  terminal = termonad-with-packages.overrideAttrs(o:  {
    extraHaskellPackages = o.extraHaskellPackages or [] ++ extraHaskellPackages;
  });
in [ terminal ]
