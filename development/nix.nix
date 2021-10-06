{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.nix-prefetch-git # downloads repo and print hash
    pkgs.niv # cli tool for keeping track of nix project dependencies
  ];
}
