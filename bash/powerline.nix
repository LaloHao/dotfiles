{ config, pkgs, ... }:
let
  cfg = config.programs.powerline-go;
in {
  programs.powerline-go.enable = true;
  programs.powerline-go.newline = true;
  programs.powerline-go.modules = [
    "cwd"
    "docker"
    "docker-context"
    "dotenv"
    "exit"
    "git"
    "jobs"
    "nix-shell"
    "node"
    "root"
    "shenv"
    "ssh"
    "termtitle"
    "time"
  ];
  programs.bash.shellAliases = {
    powerline-go = "${pkgs.powerline-go}/bin/powerline-go";
    powerline = "powerline-go";
  };
}
