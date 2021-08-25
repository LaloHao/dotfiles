{ config, lib, pkgs, ... }:

let
  inherit (lib) unbreak;
in {
  nixpkgs.overlays = [
    (self: super: {
      haskellPackages = (self.haskellPackages.override {
        HDBC-mysql = (unbreak (self.haskellPackages.HDBC-mysql.overrideAttrs (o: {
          librarySystemDepends = o.librarySystemDepends ++ [ pkgs.mysql57 ];
          broken = false;
        })));
          # unbreak super.haskellPackages.HDBC-mysql ./HDBC-mysql.patch;
      });
    })
  ];
}
