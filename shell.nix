{ pkgs ? import (builtins.fetchTarball "channel:nixos-19.09") {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    rustPackages.cargo
    rustPackages.rustc
  ];
}
