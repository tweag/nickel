# Nickel Language VS Code extension

## Features

* Syntax highlighting
* Language server
* Code formatting using [Topiary](https://github.com/tweag/topiary)

## Prerequsites

### Install `nickel`, `nls` and `topiary`

#### Using Nix

```shell
nix profile install nixpkgs#{nickel,nls,topiary}
```

#### Using Cargo

```shell
cargo install nickel-lang-cli nickel-lang-lsp
```

#### Other installation methods

See [the Nickel README](https://github.com/tweag/nickel/) and [The LSP
README](https://github.com/tweag/nickel/tree/master/lsp).

## Build from source

### With Nix

From the root of the Nickel project:

```shell
nix build .\#vscodeExtension
```

The VSIX extension will be at `./result/vscode-nickel.vsix`.

### With Yarn

From this directory:

```shell
yarn install && yarn compile && yarn vsce package --yarn
```

The VSIX extension will be at `./vscode-nickel-[version].vsix`.

## Updating `package.json`

Whenever you change `package.json`, you must run the following command:

```shell
yarn install && yarn2nix > yarn.nix
```

This will update:

* `yarn.lock`
* `yarn.nix`
