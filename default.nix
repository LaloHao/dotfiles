{ config, lib, pkgs, ... }:
let
  kmonad = import ./kmonad.nix {};
in {
  imports = [
    ./bash
    ./emacs.nix
    ./notifications.nix
    ./python
    ./neo4j.nix
    ./development
    ./graphics
    # ~/dev/x11/kmonad/nix/nixos-module.nix # does not work with home-manager
  ];

  home.packages = with pkgs; [
    xorg.xmodmap
    kmonad
    scrot
    wireshark
    tesseract4
    google-fonts
    texlive.combined.scheme-full
  ];

  services.neo4j = {
    enable = true;
    directories.home = "/tmp/neo4j/testing-home";
    extraServerConfig = ''
      dbms.unmanaged_extension_classes=org.neo4j.graphql=/graphql
    '';
  };
}
