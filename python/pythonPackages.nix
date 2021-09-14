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
    pip.scikitlearn
    pip.pillow
    pip.Keras
    pip.snakeviz
    pip.pytesseract
    pip.pillow
    # Estos paquetes colisionan entre si mismos
    pip.pyqt5.dev
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
    pip.pyglet
    pip.pyopencl
    pip.pyopengl
    pip.pyopengl-accelerate
    pip.gym
    # pip.gym-super-mario-bros
    pip.visdom
    pip.pytesseract

    # required for :lang python
    pip.pytest
    pip.nose
    pip.black
    pip.pyflakes
    pip.isort
    pip.librosa
    pip.sounddevice
    pip.soundfile
    pip.multiprocess
    pip.numba
    pip.audioread
    pip.umap-learn
    pip.unidecode
    pip.webrtcvad
    pip.inflect
    pip.pypdf3
    pip.googletrans
    pip.boto3
    # pip.ghostscript
    (pip.tesserocr.override {
      tesseract = pkgs.tesseract4;
    })

    # pip.poetry
    # pip.deep-translator
  ];
  # ] ++ (pkgs.poetry2nix.mkPoetryPackages { projectDir = "."; }).poetryPackages;
  python = pkgs.callPackage ./python.nix {
    inherit pythonPackages cudatoolkit cudnn nccl magma;
  };
in {
  home.packages = [
    python
    pkgs.qt5.full
    pkgs.qtcreator
    pkgs.conda
    pkgs.libGLU # required for opengl
    pkgs.libsndfile
  ];
}
