{ config, lib, pkgs, ... }:
{
  imports = [
    ./c.nix
    ./haskell
    ./version-control.nix
    ./static-analysis.nix
    ./javascript.nix
    ./reverse-engineering.nix
    ./nix.nix
  ];
}
