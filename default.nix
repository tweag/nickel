let
  inherit (default.inputs.nixos.lib) recurseIntoAttrs;

  default = (import (fetchTarball https://github.com/edolstra/flake-compat/archive/master.tar.gz) {
  src = builtins.fetchGit ./.;
}).defaultNix;
in
builtins.mapAttrs (_: v: recurseIntoAttrs v) default.packages // {
  shell = import ./shell.nix;
}
