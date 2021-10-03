{ pkgs ? import <nixpkgs> {
  config.allowUnfree = true;
}, pythonPackages, cudatoolkit, cudnn, nccl, magma }:
let
  opencv = pkgs.callPackage ./opencv.nix { inherit cudatoolkit; };
  tensorflow = pkgs.callPackage ./tensorflow.nix { inherit cudatoolkit cudnn nccl; };
  pytorch = pkgs.callPackage ./pytorch.nix { inherit cudatoolkit cudnn nccl magma; };
  python = let
    packageOverrides = self: super: rec {
      pyqt5 = super.pyqt5.overrideAttrs (o: rec {
        dontWrapQtApps = true;
        # passtru = o.passtru // {
        #   inherit wrapQtAppsHook;
        # };
        # preCheck = o.preCheck or "" + ''
        #   export QT_PLUGIN_PATH="${pkgs.qt5.full}/${pkgs.qt5.qtbase.qtPluginPrefix}"
        # '';

        makeWrapperArgs = o.makeWrapperArgs or [] ++ [
          ''--set QT_PLUGIN_PATH="${pkgs.qt5.full}/${pkgs.qt5.qtbase.qtPluginPrefix}"''
        ];

        pythonImportsCheck = [ "PyQt5" ];
        checkInputs = [ pkgs.python3.pytestCheckHook ];

        propagatedBuildInputs = o.propagatedBuildInputs or [] ++ [
          pkgs.qt5.full
          pkgs.qtcreator
        ];
        nativeBuildInputs = o.nativeBuildInputs ++ [
          pkgs.qt5.wrapQtAppsHook
          # pkgs.makeWrapper
        ];
        # python = self;
      });
      # deep-translator = self.callPackage ./deep-translator.nix {
      #   python = self;
      # };
      opencv4 = (super.toPythonModule (opencv.override {
        pythonPackages = self;
      }));
      tensorflowWithCuda = tensorflow super;
      pytorchWithCuda = pytorch super;
      torchvision = (super.torchvision.override {
        cudaSupport = true;
        pytorch = pytorchWithCuda;
        inherit cudatoolkit cudnn;
      });
      torchfile = self.callPackage ./torchfile.nix { };
      visdom = self.callPackage ./visdom.nix { inherit torchfile; };
      nes-py = self.callPackage ./nes-py.nix { };
      gym-super-mario-bros = self.callPackage ./gym-super-mario-bros.nix {};
      pytesseract = super.pytesseract.override {
        tesseract = pkgs.tesseract4;
      };
      pygobject = self.pygobject3;
      notify-send.py = self.toPythonModule (self.callPackage ./notify-send.nix { });
    }; in
    # (pkgs.poetry2nix.mkPoetryPackages {
    #   projectDir = "/home/hao/dev/dotfiles/python";
    #   overrides = packageOverrides;
    # }).python;
      pkgs.python3.override {
      # enableOptimizations = true;
      # reproducibleBuild = false;
      inherit packageOverrides;
      self = python;
    };
  # python' = let
  #   packageOverrides = (pkgs.poetry2nix.mkPoetryPackages { projectDir = "./."; })
  # inherit (pkgs.poetry2nix.mkPoetryPackages {
  #   projectDir = "/home/hao/dev/dotfiles/python";
  #   inherit python;
  # }) poetryPackages;
  # poetryPackages' = builtins.listToAttrs (builtins.map (x: { name = "${x.name}"; value = x; }) poetryPackages);
  # deep-translator = poetryPackages'."python3.9-deep-translator-1.5.4";
# in (python.withPackages (p: (pythonPackages p) ++ [deep-translator]))
in (python.withPackages pythonPackages)
