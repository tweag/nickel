{ fromGit ? true }:
(import (fetchTarball https://github.com/edolstra/flake-compat/archive/master.tar.gz) {
  src = if fromGit then builtins.fetchGit ./. else ./.;
}).defaultNix
