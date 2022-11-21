# Nickel Language Server Protocol VS Code extension

This directory contains the Visual Studio Code Nickel LSP extension code.

## Build

### With Nix

From the root of the Nickel project:

```shell
nix build .\#vscodeExtension.vsix
```

The VSIX extension will be at `./result-vsix/nls-client.vsix`.

### With NPM

From this directory:

```shell
npm install && npm run compile && echo y | npx vsce package
```

The VSIX extension will be at `./nls-[version].vsix`.

## Updating `package.json`

Whenever you change `package.json`, you must run the following command:

```shell
npm install --package-lock-only && node2nix --development --lock
```

This will update:

* `package-lock.json`
* `default.nix`
* `node-env.nix`
* `node-packages.nix`
