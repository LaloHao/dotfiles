{ haskellPackages }:
haskellPackages.ghcWithHoogle(p: with p; [
  transformers
  text
  lens
  dbus
  # xmonad
  # xmonad-contrib
  # xmonad-extras
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
  persistent-sqlite
])
