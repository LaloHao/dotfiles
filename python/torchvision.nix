{ pytorch, cudatoolkit, cudnn }:

(self: self.torchvision.override {
  cudaSupport = true;
  inherit pytorch cudatoolkit cudnn;
})
