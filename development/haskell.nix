{ config, pkgs, ... }:

let
  cudatoolkit = pkgs.cudatoolkit_11;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # accelerate with cuda 11 builds but crashes? (i can't remember)
      accelerate-llvm-native = dontCheck super.accelerate-llvm-native;
      accelerate-llvm-ptx    = dontCheck super.accelerate-llvm-ptx;
      accelerate-fft  	     = dontCheck super.accelerate-fft;
    };
  };
  haskellEnv = haskellPackages.ghcWithHoogle(p: with p; [
    text
    lens
    xmonad
    xmonad-contrib
    xmonad-extras
    accelerate
    accelerate-llvm-ptx
    cuda
    OpenGL
    GLFW-b
    GLUT
    JuicyPixels
    JuicyPixels-extra
    GLUtil
    bmp
    foreign-store
    yesod
    warp
  ]);
in {
  home.packages = [
    haskellEnv
    cudatoolkit
  ];
}
