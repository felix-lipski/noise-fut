{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  customPython = pkgs.python39.buildEnv.override {
    extraLibs = with pkgs.python39Packages; [
      numpy
      pillow
      pyopencl
    ];
  };
in
[ customPython ] ++ 
[
  futhark

  opencl-headers
  # opencl-icd
  opencl-info
  # ocl-icd
  clinfo
]
