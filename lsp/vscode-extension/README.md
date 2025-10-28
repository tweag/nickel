# Nickel Language VS Code extension

## Features

* Syntax highlighting
* Nickel Language Server (NLS) integration
* Code formatting (through NLS)

## Prerequisites

The VSCode extension is [available on the VSCode Market
Place](https://marketplace.visualstudio.com/items?itemName=Tweag.vscode-nickel).
You need the Nickel Language Server (NLS) installed independently from the
VSCode extension. Installation methods for NLS are detailed below.

### Install NLS

#### Using pre-built binaries

Each Nickel release comes with pre-built binaries including NLS for a few common
platforms. You can download them from [the GitHub releases
page](https://github.com/tweag/nickel/releases).

If your platform is not supported, you can try the methods below. You should
prefer [Nix](https://nixos.org/) or [Cargo](https://doc.rust-lang.org/cargo/) if
you already have on of them installed. Otherwise, using Cargo might be the
simplest option.

#### Using Nix

```console
nix profile install nixpkgs#nls
```

#### Using Cargo

```console
cargo install nickel-lang-lsp
```

#### Other installation methods

See [the Nickel README](https://github.com/tweag/nickel/) and [The LSP
README](https://github.com/tweag/nickel/tree/master/lsp).

## Building from source

If you need to build the VSCode extension yourself, you'll need to build it from
source. Different methods are detailed below.

### With Nix

From the root of the Nickel project:

```console
nix build .\#vscodeExtension
```

The VSIX extension will be at `./result/vscode-nickel.vsix`.

### With Yarn

From this directory:

```console
yarn install && yarn compile && yarn vsce package --yarn
```

The VSIX extension will be at `./vscode-nickel-[version].vsix`.

## Updating `package.json`

Whenever you change `package.json`, you must run the following command:

```shell
yarn install
```

This will update `yarn.lock`.
