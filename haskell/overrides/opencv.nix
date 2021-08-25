{ pkgs }:
let
  opencv3 = pkgs.opencv3.override(old: {
    enableGtk2 = true;
    enableFfmpeg = true;
    enableGStreamer = true;
    enablePython = true;
  });
in (self: super: {
  haskellPackages = (super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      opencv = (super.haskellPackages.opencv.overrideAttrs (o: {
        librarySystemDepends = o.librarySystemDepends ++ [ opencv3 ];
      }));
    };
  });
})
