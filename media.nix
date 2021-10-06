{ config, lib, pkgs, ... }:

{
  # download music or videos
  programs.bash.shellAliases = {
    "bestaudio" = "youtube-dl -f bestaudio";
    "bestvideo" = "youtube-dl -f bestvideo";
  };
}
