{ config, pkgs, ... }:
let
  cudatoolkit = pkgs.cudaPackages.cudatoolkit_11;
  cudnn = pkgs.cudnn_cudatoolkit_11;
  nccl = pkgs.nccl_cudatoolkit_11;
  magma = pkgs.magma.override { inherit cudatoolkit; };
  pythonPackages = pip: [
    pip.tensorflowWithCuda
    pip.opencv4
    pip.pip
    pip.virtualenv
    pip.matplotlib
    pip.numpy
    pip.scikitimage
    pip.pillow
    pip.snakeviz
    pip.pytesseract
    pip.pillow
    # Estos paquetes colisionan entre si mismos
    # pip.pyqt5_with_qtmultimedia
    # pip.pyqt5_with_qtwebkit
    pip.tqdm
    pip.lmdb
    pip.pyqt-builder
    pip.pyqtgraph
    # pip.conda
    pip.pyqtwebengine
    (pip.imutils.override (o: {
      opencv3 = pip.opencv4;
    }))
    pip.notebook
    pip.pytorchWithCuda
    pip.torchvision
    # pip.torchaudio # broken or unexistant?
    pip.seaborn
  ];
  python = pkgs.callPackage ./python.nix {
    inherit pythonPackages cudatoolkit cudnn nccl magma;
  };
in {
  home.packages = [
    python
    pkgs.qt5.full
    pkgs.qtcreator
    pkgs.conda
  ];
}
