{ config, lib, pkgs, ... }:
{
  home.packages = [
    pkgs.nodePackages.lerna
  ];
}
