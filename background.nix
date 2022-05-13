{ config, lib, pkgs, ... }:

{
  services.random-background = {
    enable = true;
    imageDirectory = "/home/hao/Pictures/wall";
    display = "scale";
    interval = "5m";
    enableXinerama = true;
  };
}
