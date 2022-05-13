{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.bind  # nslookup, dig
  ];
}
