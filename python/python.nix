{ pkgs ? import <nixpkgs> {
  config.allowUnfree = true;
}, pythonPackages, cudatoolkit, cudnn, nccl, magma }:
let
  opencv = pkgs.callPackage ./opencv.nix { inherit cudatoolkit; };
  tensorflow = pkgs.callPackage ./tensorflow.nix { inherit cudatoolkit cudnn nccl; };
  pytorch = pkgs.callPackage ./pytorch.nix { inherit cudatoolkit cudnn nccl magma; };
  python = let
    packageOverrides = self: super: rec {
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
      nes-py = self.callPackage ./nes-py.nix { };
      gym-super-mario-bros = self.callPackage ./gym-super-mario-bros.nix {};
    }; in pkgs.python3.override {
      inherit packageOverrides; self = python;
    };
in python.withPackages pythonPackages
