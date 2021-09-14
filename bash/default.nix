{ config, lib, pkgs, ... }:
let
  inherit (builtins) readFile;
in {
  imports = [
    ./alias.nix
    ./direnv.nix
    ./powerline.nix
  ];

  programs.bash.enable = true;
  programs.bash.enableVteIntegration = true;
  programs.bash.historyControl = [
    # "erasedups"   # dont save if command is already in history
    # "ignoredups"  # dont display duplicated lines
    # "ignorespace" # dont display lines with leading space
  ];
  programs.bash.historyFile = "$HOME/.bash_history.1";
  programs.bash.historyFileSize = 100000;
  programs.bash.historySize = 100000;
  programs.bash.historyIgnore = [
    # "ls"
    # "cd"
    # "exit"
  ];

  # Extra commands that should be run when initializing an interactive shell.
  programs.bash.initExtra =
    lib.concatStringsSep "\n" [
      (readFile ./bashrc)
      # "echo ${config.home.homeDirectory}"
    ];
  # Extra commands that should be run when logging out of an interactive shell.
  programs.bash.logoutExtra = "";
  # Extra commands that should be run when initializing a login shell.
  programs.bash.profileExtra = readFile ./profile;
  # Environment variables that will be set for the Bash session.
  programs.bash.sessionVariables = {
    XDG_DATA_DIRS = ''$HOME/.nix-profile/share:$HOME/.share:"''${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"'';
  };

  # Shell options to set.
  programs.bash.shellOptions = [
    "histappend"
    "checkwinsize"
    "extglob"
    "globstar"
    "checkjobs"
  ];
}
