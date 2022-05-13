{ cudatoolkit, cudnn, nccl, magma }:

(self: self.pytorchWithCuda.override {
  cudaSupport = true;
  cudaPackages = {
    inherit cudatoolkit cudnn nccl;
  };
})
