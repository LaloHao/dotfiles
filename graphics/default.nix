{ config, lib, pkgs, ... }:

{
  imports = [
    ./window-manager.nix
  ];

  home.packages = [
    pkgs.xorg.xdpyinfo
    pkgs.xorg.xwininfo
    pkgs.xorg.xev
    pkgs.ark
    pkgs.gsimplecal
  ];
}
