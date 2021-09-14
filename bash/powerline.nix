{ config, pkgs, ... }:
let
  cfg = config.programs.powerline-go;
in {
  programs.powerline-go.enable = true;
  programs.powerline-go.newline = true;
  programs.powerline-go.modules = [
    "exit"
    "time"
    "ssh"
    "nix-shell"
    "jobs"
    "dotenv"
    "cwd"
    "git"
    "node"
    "root"
    "shenv"
  ];
  programs.powerline-go.settings = {
    "cwd-max-depth" = 2;
  };

  programs.bash.shellAliases = {
    powerline-go = "${pkgs.powerline-go}/bin/powerline-go";
    powerline = "powerline-go";
  };
}
