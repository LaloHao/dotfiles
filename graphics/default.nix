{ config, lib, pkgs, ... }:

{
  imports = [
  ];

  home.packages = [
    pkgs.xorg.xdpyinfo
    pkgs.xorg.xwininfo
    pkgs.xorg.xev
    pkgs.ark
    pkgs.gsimplecal
  ];
}
