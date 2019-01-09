{ pkgs ? import (builtins.fetchTarball "channel:nixos-18.09") {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    rust.cargo
    rust.rustc
  ];
}
