{ pkgs ? import (builtins.fetchTarball "channel:nixos-19.03") {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    rust.cargo
    rust.rustc
  ];
}
