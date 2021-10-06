{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.codeql
  ];
}
