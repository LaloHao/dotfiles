{ config, lib, pkgs, ... }:
let
  unstable = import <unstable> { config.allowUnfree = true; };
in {
  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      haskellPackages = unstable.haskellPackages;
      extraPackages = haskellPackages: with haskellPackages; [
        (import ../development/haskell/haskellPackages.nix { inherit haskellPackages; })
      ];
    };
  };
}
