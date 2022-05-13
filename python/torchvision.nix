{ pytorch, cudatoolkit, cudnn }:

(self: self.torchvision.override {
  cudaSupport = true;
  cudaPackages = {
    inherit cudatoolkit cudnn nccl;
  };
})
