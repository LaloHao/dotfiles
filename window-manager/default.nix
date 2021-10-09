{ config, lib, pkgs, ... }:
let
  unstable = import <unstable> {
    config = {
      allowUnfree = true;
      # allowBroken = true;
    };
  };
  mkLibFile = v: { name = "${v}.hs"; value = ./. + "/lib/${v}.hs"; };
  mkLibFiles = files: lib.listToAttrs (lib.lists.map mkLibFile files);
in {
  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      haskellPackages = unstable.haskellPackages;
      extraPackages = haskellPackages: with haskellPackages; [
        haskellPackages.lens
        (import ../development/haskell/haskellPackages.nix { inherit haskellPackages; })
      ];
      libFiles = mkLibFiles [
        "XMobar/Task"
        "XMonad/Hao"
      ];
    };
  };
  home.file.".xmonad/xmonad.hs" = {
    source = ./xmonad.hs;
    onChange = ''
      ${config.xsession.windowManager.command} --recompile
    '';
  };
}
