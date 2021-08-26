{ pkgs ? import <nixpkgs> {  } }:
let
  kmonad-bin = pkgs.fetchurl {
    url = "https://github.com/kmonad/kmonad/releases/download/0.4.1/kmonad-0.4.1-linux";
    sha256 = "13vs7xq9clgg6pd9gr49h5ljgyg0kc63qd3ghh3dvmi3rkkmi7l3";
  };
in
pkgs.runCommand "kmonad" {} ''
  #!${pkgs.stdenv.shell}
  mkdir -p $out/bin
  cp ${kmonad-bin} $out/bin/kmonad
  chmod +x $out/bin/*
''
