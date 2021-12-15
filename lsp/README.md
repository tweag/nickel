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

## Client


### VS Code

Until nls is available through the vscode marketplace, it can be built using nix:

Either using the command line

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
