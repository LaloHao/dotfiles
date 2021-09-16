{ config, lib, pkgs, ... }:

{
  programs.bash.shellAliases = {
    ".." = "cd ..";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    "....." = "cd ../../../..";
    "......" = "cd ../../../../..";
    dl = "download";
    gc = "git clone";
    open = "xdg-open";
    clip = "xclip -sel clip";
    # mv = "rsync -avzh --delete-source"; # nope
    feh = "feh --scale-down --auto-rotate --auto-zoom --reverse";
  };
}
