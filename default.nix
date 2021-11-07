{ pkgs, nixpkgs }:
let
  lib          = nixpkgs.lib;
in
pkgs.pkgs.stdenv.mkDerivation rec {
  pname        = "noise-fut";
  version      = "0.0.0";
  dontUnpack   = true;
  buildInputs  = (import ./build-inputs.nix) { inherit pkgs; };

  buildPhase   = ''
    echo "example file" > foo.txt
    '';

  installPhase = ''
    mkdir -p $out/bin
    cp foo.txt $out/bin/foo.txt
    '';
}
