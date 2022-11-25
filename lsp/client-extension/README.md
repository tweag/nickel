# Nickel Language Server Protocol VS Code extension

This directory contains the Visual Studio Code Nickel LSP extension code.

## Build

### With Nix

From the root of the Nickel project:

```shell
nix build .\#vscodeExtension
```

The VSIX extension will be at `./result/nls-client.vsix`.

### With Yarn

From this directory:

```shell
yarn install && yarn run compile && echo y | yarn exec vsce package --yarn
```

The VSIX extension will be at `./nls-[version].vsix`.
