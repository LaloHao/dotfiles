{ pkgs, cudatoolkit, cudnn, nccl }:

(self: self.tensorflowWithCuda.override (o: {
  cudaSupport = true;
  inherit cudatoolkit cudnn nccl;
}))
