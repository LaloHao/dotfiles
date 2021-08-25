{ pkgs ? import <nixpkgs> {
  config.allowUnfree = true;
}, ... }: {
  home.packages = [
    pkgs.android-studio
  ];
}
