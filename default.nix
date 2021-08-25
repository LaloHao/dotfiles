{ config, lib, pkgs, ... }:
{
  imports = [
    ./bash
    ./emacs.nix
    ./xbindkeys.nix
    ./notifications.nix
    ./compositor.nix
    ./python
    ./neo4j.nix
    ./development
    # ./window-manager.nix
  ];

  home.packages = with pkgs; [
    xorg.xmodmap
  ];

  services.neo4j = {
    enable = true;
    directories.home = "/tmp/neo4j/testing-home";
    extraServerConfig = ''
      dbms.unmanaged_extension_classes=org.neo4j.graphql=/graphql
    '';
  };
}
