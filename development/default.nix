{ config, lib, pkgs, ... }:
{
  imports = [
    ./c.nix
    ./haskell
  ];
}
