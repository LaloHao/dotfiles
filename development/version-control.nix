{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.gh # github-cli
  ];
}
