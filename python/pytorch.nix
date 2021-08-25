{ cudatoolkit, cudnn, nccl, magma }:

(self: self.pytorchWithCuda.override {
  cudaSupport = true;
  inherit cudatoolkit cudnn nccl magma;
})
