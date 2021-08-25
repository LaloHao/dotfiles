{ pkgs, pythonPackages, cudatoolkit }:

pkgs.opencv4.override {
  inherit cudatoolkit pythonPackages;
  enableGtk2 = true;
  enableFfmpeg = true;
  enableGStreamer = true;
  enablePython = true;
  enableCuda = true;
  enableTesseract = true;
  enableTbb = true;
  enableOvis = true;
  enableGPhoto2 = true;
  enableDC1394 = true;
  enableDocs = true;
}
