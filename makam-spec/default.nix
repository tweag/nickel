{pkgs ? import <nixpkgs> {
    inherit system;
  }, system ? builtins.currentSystem, nodejs ? pkgs."nodejs-10_x"}:

let
  nodePackages = import ./makam-composition.nix {
    inherit pkgs nodejs;
    inherit (pkgs.stdenv.hostPlatform) system;
  };
in
nodePackages // {
  makam =  nodePackages.makam.override {
    buildInputs = [ pkgs.nodejs pkgs.makeWrapper ];
    postFixup = ''
      wrapProgram "$out/bin/makam" --prefix PATH : ${pkgs.stdenv.lib.makeBinPath [ pkgs.nodejs ]}
      patchelf --set-interpreter ${pkgs.stdenv.glibc}/lib/ld-linux-x86-64.so.2 "$out/lib/node_modules/makam/makam-bin-linux64"
    '';
  };
}
