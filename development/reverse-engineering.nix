{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.flashrom   # flashing/reading firmware
    pkgs.binwalk    # analyzing/extracting firmware
    pkgs.tcpdump    # inspecting network traffic
  ];
}
