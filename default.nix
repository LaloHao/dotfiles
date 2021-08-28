{ config, lib, pkgs, ... }:
{
  imports = [
    ./bash
    ./emacs.nix
    ./xbindkeys.nix
    ./notifications.nix
    ./python
    ./neo4j.nix
    ./development
    ./graphics
  ];

  home.packages = with pkgs; [
    xorg.xmodmap
    (import ./kmonad.nix {})
  ];

  services.neo4j = {
    enable = true;
    directories.home = "/tmp/neo4j/testing-home";
    extraServerConfig = ''
      dbms.unmanaged_extension_classes=org.neo4j.graphql=/graphql
    '';
  };
}
