# Nickel Language Server

NLS is a language server for the nickel programming language.  NLS offer error messages, type hints, and auto-completion right in your favorite LSP-enabled editor.

## Server
  
To be editor independent `nls` is made up of two components the server and possible lsp-client configurations that talk to the server directly.
Hence, the server has to be installed separately to the client configuration. 

### Installation

The easiest way to install `nls` is using nix. `nls` is installed in the same installation as `nickel`:


To enter a shell with `nickel` and `nls` run

```
nix shell github:tweag/nickel
```

To install `nickel` and `nls` into your profile run

```
nix profile install github:tweag/nickel
```

To build the `nickel` and `nls` executables run

```
# The executables will be placed in ./result/bin/
nix build github:tweag/nickel
```

Alternatively to install without nix flakes using nix run:

```
git clone https://github.com/tweag/nickel.git
cd nickel
nix-env -f . -i
```

## Client


### VS Code

NLS is currently not available through the vscode marketplace. In the meantime, it can be built using Nix.

Either using the command line (you need to have the [`jq`](https://stedolan.github.io/jq/) command available)

```
code --install-extension $(nix build ./\#vscodeExtension --no-link --json | jq ".[0].outputs.vsix")
```

or in two steps, by first building the extension

```
nix build github:tweag/nickel#vscodeExtension.vsix
```

then installing it using `Extension: Install from VSIX` in the vscode command palette and choosing `./result-vsix/nls-client.vsix`.

#### Configuration

The VS Code extension offers three configuration options: 

- `"nls.server.path"`: Path to nickel language server
- `"nls.server.trace"`: "Enables performance tracing to the given file"
- `"nls.server.debugLog"`: "Logs the communication between VS Code and the language server." 

### Neovim

After installing `nickel` and `nls`
`nls` is supported in [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig).
Using `nvim-lspconfig` setup `nls` like all your other LSP servers as described by the `nvim-lspconfig` ReadMe.

```lua
require('lspconfig')["nickel_ls"].setup {}
```

### Neo(Vim) with Coc.nvim

Add the `nickel_ls` `JSON` object to your `:CocConfig`/`coc-settings.json`.
```
{
  "languageserver": {
    "nickel_ls": {
      "command": "nls",
      // You can enable performance tracing with:
      // "command": "nls --trace <file>",
      "rootPatterns": [
        ".git"
      ],
      "filetypes": [
        "ncl"
      ]
    }
  }
}
```
