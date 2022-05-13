{ pkgs, cudatoolkit, cudnn, nccl }:

(self: self.tensorflowWithCuda.override (o: {
  cudaSupport = true;
  cudaPackages = {
    inherit cudatoolkit cudnn nccl;
  };
}))
