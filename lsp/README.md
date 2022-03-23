# Nickel Language Server

The Nickel Language Server (NLS) is a [language
server](https://en.wikipedia.org/wiki/Language_Server_Protocol) for the
[Nickel](https://www.nickel-lang.org/) programming language. NLS offers error
messages, type hints, and auto-completion right in your favorite LSP-enabled
editor.

NLS is a stand-alone binary. Once built, you must then configure you code editor
to use it for Nickel source files. This document covers building NLS and using
it in VSCode and (Neo)Vim.

## Installation

Three installation methods are proposed: using Nix flakes (**recommended**),
using Nix without flakes (older Nix versions), or using `cargo` if you already
use the Rust toolchain and don't want to install Nix.

### Using Nix (flakes)

The easiest way to install `nls` is using [Nix](https://nixos.org/).

**Important**: the following of this section assumes that you have a flake-enabled
Nix (>= 2.4) and the experimental features `flakes` and `nix-command` enabled.
At the time of writing, the current stable version of Nix is flake-enabled. If
you haven't enabled any experimental feature globally or don't know what it is
even about, just append `--experimental-features "flakes nix-command"` to all of
the following commands.

- *Global installation*: if you want to use `nls` on a regular basis, this is
  what you want to do. To have `nickel` and `nls` available globally, add them
  into your profile:
  ```
  nix profile install github:tweag/nickel
  ```
- *Shell*: Try out for the time of a session. To be dropped in a shell with the
  `nickel` and `nls` commands available, run:
  ```
  nix shell github:tweag/nickel
  ```
- *Local build*: if you just wand to build `nickel`
  and `nls` without installing them:
  ```
  # The executables will be placed in ./result/bin/
  nix build github:tweag/nickel
  ```

### Using Nix (without flakes)

Alternatively, you can insall `nickel` and `nls` globally on older Nix versions
without flakes via `nix-env`:

```
git clone https://github.com/tweag/nickel.git
cd nickel
nix-env -f . -i
```

### Using Cargo

If you already have a working [`cargo`](https://doc.rust-lang.org/cargo/)
installation, you can make `nls` available globally without Nix:

```
cargo install nickel-lang-lsp
```

**WARNING**: the 0.1.0 version of the NLS crate
([nickel-lang-lsp](https://crates.io/crates/nickel-lang-lsp)) doesn't
correctly define the name of the binary as `nls`. If you can't find `nls` after
a successful cargo installation, try to run `nickel-lang-lsp --version`. If this
command is available, you'll have to substitute `nls` for `nickel-lang-lsp` in
the instructions that follow.

## Interfacing with editors

Once the `nls` binary is available, you can proceed with the configuration of
your editor.

### VS Code

#### Build the extension

NLS is currently not available through the vscode marketplace, but this
repository includes an extension that can be built locally via Nix (cf []()
about the Nix setup).

- One-liner (using the [`jq`](https://stedolan.github.io/jq/) command):
  ```
  code --install-extension $(nix build ./\#vscodeExtension --no-link --json | jq ".[0].outputs.vsix")
  ```
- In two steps, going via VSCode:
  - Build with Nix:
    ```
    nix build github:tweag/nickel#vscodeExtension.vsix
    ```
  - Then, in VSCode, use `Extension: Install from VSIX` in the vscode command
    palette and choose `./result-vsix/nls-client.vsix`.

#### Configuration

The VS Code extension offers three configuration options: 

- `nls.server.path`: Path to nickel language server
- `nls.server.trace`: Enables performance tracing to the given file
- `nls.server.debugLog`: Logs the communication between VS Code and the language server.

### (Neo)Vim

Before proceeding install the [Nickel syntax highlighting plugin](https://github.com/nickel-lang/vim-nickel) using your Vim plugin manager.
Without this plugin your LSP client may not start NLS on nickel source files.

With Vim-Plug:
```vim
Plug 'nickel-lang/vim-nickel'
```

#### Neovim builtin LSP

`nls` is supported in
[nvim-lspconfig](https://github.com/neovim/nvim-lspconfig). Using
`nvim-lspconfig` setup `nls` like all your other LSP servers as described by the
`nvim-lspconfig` ReadMe.

```lua
require('lspconfig')["nickel_ls"].setup {}
```

#### With Coc.nvim

Add an `nickel_ls` entry to your configuration. Type `:CocConfig` in Neovim (or edit `coc-settings.json`) and add:

```
{
  "languageserver": {
    # Your other language servers configuration
    # ...,
    "nickel_ls": {
      "command": "nls",
      # You can enable performance tracing with:
      # "command": "nls --trace <file>",
      "rootPatterns": [
        ".git"
      ],
      "filetypes": [
        "ncl"
      ]
    },
  }
}
```

### Emacs

TODO
