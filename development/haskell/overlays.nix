self: super:
{
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: with self.haskell.lib; {
      # accelerate with cuda 11 builds but crashes? (i can't remember)
      accelerate-llvm-native = dontCheck haskellSuper.accelerate-llvm-native;
      accelerate-llvm-ptx    = dontCheck haskellSuper.accelerate-llvm-ptx;
      accelerate-fft  	     = dontCheck haskellSuper.accelerate-fft;
    };
  };
}
