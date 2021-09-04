{ config, pkgs, ... }:
{
  nixpkgs.overlays = [
    # (self: super: { haskellPackages = import ./overlays.nix { inherit pkgs; }; })
    (import ./overlays.nix)
  ];

  home.packages = [
    (import ./haskellPackages.nix { inherit (pkgs) haskellPackages; })
    pkgs.cudatoolkit_11
    pkgs.picom
  ];
}
