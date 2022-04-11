Contributing to Nickel
======================

Welcome, and thanks for considering contributing to the Nickel project!

# Contributing

There are many useful ways to help which don't involve modifying the source of
Nickel:

- Improving the documentation, user-facing or internal, technical or high-level
- Growing the Nickel ecosystem, by providing the community with libraries
  (e.g. a collection of contracts for new use cases), augmenting the
  standard library, or improving the tooling (such as code editor integration).
- Reviewing changes from other contributors.

The rest of this document is concerned with any changes impacting the `nickel`
repository, which may or may not involve changing the source of Nickel.

# Table of content

1. [Preamble](#preamble)
1. [Resources](#resources)
1. [Setup a development environment](#set-up-a-development-environment)
1. [How to submit changes](#how-to-submit-changes)

# Preamble

Before contributing any non trivial change to this repository, please first
check for the existence of related work such as issues or open pull requests. If
there are none, it's better to discuss the change you wish to make via an issue,
by email, or using any other method with the maintainers of this repository
(listed below) before actually submitting something.

# Resources

## Documentation

The following resources are oriented toward Nickel users:

- The [README](./README.md) and the [design rationale](./RATIONALE.md)
- The [blog post serie][blog-serie] and the [release blog post][blog-release].
- The [user manual][user-manual].

For Nickel contributors (or aspiring contributors), the following technical
documentation is relevant:

- The [crate documentation][doc-crate].
- The [RFCs][rfcs]. There is currently no well established process for RFCs, but as a
  rule of thumb, impactful changes to the design or the implementation of the
  language are technically discussed and documented in a dedicated RFC document.
- The [technical notes][doc-notes]. Various notes gathering thoughts, proposals,
  or issues about an aspect of the language at one point in time that the author
  thought important to keep for posterity. They are usually more informal than
  RFCs, of smaller scope, and their content may become obsolete more easily.

## People

Nickel is maintained by [Tweag][tweag]. The current team of maintainers is
comprised of:

- Yann Hamdaoui (@yannham)
- François Caddet (@francoiscaddet)
- Erin Van Der Veen (@...)
- Steven Shaw (@...)

You can find some of us on our [matrix channel][matrix-nickel] (and in
particular the Devs room), or fire an email at `nickel-lang@protonmail.com`.

# Set up a development environment

There are two ways to set up a development environment for Nickel: using
[Nix][nix], or directly via your preferred system package manager. Nix is able
to drop you in a development shell with everything needed (the Rust toolchain,
mostly) to hack on Nickel in one command, without installing anything globally
on your system. _While Nix is also capable of building Nickel by itself, using
cargo (either a system-wide installation or the one given by the Nix development
shell) is the recommended way of building when working on the Nickel repository
itself_. The reason is that incremental compilation for Rust and Nix is not
there yet, and incremental rebuilds using only Nix are going to be painfully
long.

## Content

The Nickel repository consist in 3 crates:

- `nickel-lang` (path: `.`). The main crate containing the interpreter as a library as well as the `nickel` binary.
- `nickel-lang-lsp` (path: `lsp/nls/`). the Nickel Language Server (NLS), an LSP server for Nickel.
- `nickel-lang-utilities`: (path: `utilities/`). An auxiliary crate regrouping
   helpers for tests and benchmarks. Not required to build `nickel` itself.

Other noteworthy items:

- The user manual in `doc/manual/`, as a bunch of markdown files.
- A VSCode extension for NLS in `lsp/client-extension/`.

## Using Nix

To set up a development environment using a recent Nix (>= 2.4):

1. Clone the repository: `git clone git@github.com:tweag/nickel.git`
2. At the root of the repository, run `nix develop`. You should now be dropped
   in a shell with all the required tool to hack on Nickel (`rust`, `cargo`,
   etc.)

## Without Nix

Otherwise, you can install the Rust toolchain in a standard way:

1. [Install Rust][install-rust]
2. [Install Cargo][install-cargo]

## Building

### Nickel

To build the main crate, just run `cargo build` at the root of this repository.
Upon success, the binary will be available somewhere inside the `target`
directory.

You can make the binary available in your path via `cargo install` at the root
of this repository:

```shell
$ cargo install --path .
$ nickel --version
nickel-lang 0.1.0
```

### NLS (nickel-lang-lsp)

To build NLS, the LSP server for Nickel, build the crate inside `lsp/nls`:

```shell
$ cd lsp/nls
lsp/nls $ cargo build
```

Like for Nickel, you can make the `nls` binary available in your path via `cargo
install`:

```shell
$ cargo install --path .
$ nls --version
nickel-lang-lsp 0.1.0
```

### WebAssembly REPL

There is a WebAssembly (WASM) version of the REPL, which
is used for the online playground on [nickel-lang.org][nickel-lang.org]. While
using `cargo` directly is the recommended way for building, both methods have
advantages for the WASM REPL. Building through Cargo alone is possible, but
requires installing new tools and patching the `Cargo.toml`. On the other hand,
`Nix` can perform the build in one simple command, but incremental compilation
is not as good as with direct usage of `cargo`.

Both methods are described below.

#### Using Nix

At the root of the repository, run:

```shell
$ nix build .#buildWasm
```

The corresponding NPM package is put in `result/`.

#### Using Cargo

1. [Install `wasm-pack`][install-wasm-pack]
2. Add the following line to `Cargo.toml` under the `[lib]` heading:

   ```diff
   [lib]
   +crate-type = ["cdylib", "rlib"]
   ```

3. Remove the following line from `Cargo.toml` (dependency on
   `nickel-lang-utilities`):

   ```diff
   -nickel-lang-utilities = {path = "utilities", version = "0.1.0"}
   ```

4. Run `wasm-pack`:

   ```shell
   wasm-pack build -- --no-default-features --features repl-wasm
   ```

   A `pkg` directory, containing the corresponding NPM package, should now be
   available.
5. (Optional) the generated NPM package is named `nickel`, but this name is not
   very descriptive, and is already in use in the NPM registry. You can patch
   the name of the NPM package using `jq` to be `nickel-repl` instead:

   ```shell
   $ jq '.name = "nickel-repl"' pkg/package.json > package.json.patched \
     && rm -f pkg/package.json \
     && mv package.json.patched pkg/package.json
   ```

   If you don't have `jq`, you can do it by hand: replace the `name` attribute
   in `pkg/package.json` by `"nickel-repl"`.

The `pkg` directory now contains a `nickel-repl` NPM package that can be used
from JavaScript.

## Testing

Tests are run via `cargo test`. They are two types of tests:

- Unit tests, located directly in the corresponding module.
- Integration tests, located in the dedicated crate `tests`.

You can take inspiration from the existing tests to add your own. By convention,
tests expected to pass are written in a standalone Nickel file in `tests/pass/`.
Each `.ncl` file defines a list of expressions that must individually evaluate
to the boolean `true`. The whole file is an expression that returns true if and
only if every tests pass, or fail with a contract failure to help locating the
failing test (instead of returning just `false`).

If a test expected to pass is failing, run it directly using nickel with `nickel
-f tests/pass/test_that_doesnt_pass.ncl` to get better error messages than
`cargo test`.

Tests expected to fail are often embedded directly into rust source code,
because you usually want to additionally check that the error is the one you
expect.

## Benchmarking

If your change is likely to impact performance, it is recommended to run the
benchmark suite on master and on your branch to assess any performance changes.
Please report your findings in the description of the PR.

The benchmark suite is located in the `benches/` directory. To run it:

```
cargo bench
```

Note that a full run takes some time, up to a dozen of minutes. You can run
specific benchmarks instead of the full suite. Please refer to the documentation
of [`cargo bench`][doc-cargo-bench].

# How to submit changes

Once you have made sure the maintainers are aware of your changes, you can start
the implementation, eventually submitting changes as a pull request.

**Try to keep pull requests small and focused**. Avoid packing refactoring,
cosmetic changes or anything not directly related to your original goal in the
same pull request. If preliminary steps make sense as standalone changes, don't
hesitate to split your pull request into several ones.

1. Create a new topic branch: `git checkout -b feature/adding-some-stuff`
2. Implement your changes and commit them. Try to keep commits focused as well.
3. Documentation: If you added new items (functions, modules) to the public API
   of a crate, please document those items in-code. If you made an user-facing
   change (syntax, stdlib, language features, etc.), please update the existing
   documentation (in particular the user-manual) in consequence.
4. Tests: be it for bug fixing or adding new features, try to write extensive
   tests as much as possible, to ensure the correctness of your changes as well
   as avoiding regressions.
5. Make sure your PR will pass the CI:
    * run `cargo test` on the root crate `nickel-lang`
    * run `cargo check` inside auxiliary crates: `lsp/nls` and
        `utilities/`, to make sure that your change didn't impact them
    * run `cargo fmt` at the root of each modified crate
    * (optional) check that the WebAssembly REPL is building. See [building the
      WebAssembly REPL](#webassembly-repl). This step may take some time, and
      should only be required if you added, upgraded or downgraded
      dependencies.
6. Once all the previous steps are completed, you can push your branch to your
   fork and make a pull request on GitHub.



[cachix-nickel]: https://app.cachix.org/cache/nickel
[blog-serie]: https://www.tweag.io/blog/2020-10-22-nickel-open-sourcing/
[blog-release]: https://www.tweag.io/blog/2022-03-11-nickel-first-release/
[user-manual]: https://nickel-lang.org/user-manual/introduction/
[doc-crate]: https://docs.rs/nickel-lang/0.1.0/nickel_lang/
[doc-notes]: notes/
[install-rust]: https://www.rust-lang.org/tools/install
[install-cargo]: https://doc.rust-lang.org/cargo/getting-started/installation.html
[install-wasm-pack]: https://rustwasm.github.io/wasm-pack/installer/
[doc-cargo-bench]: https://doc.rust-lang.org/cargo/commands/cargo-bench.html
[tweag]: https://www.tweag.io
[rfcs]: ./rfcs/
[matrix-nickel]: https://matrix.to/#/#nickel-lang:matrix.org
[nix]: https://nixos.org/
[nickel-lang.org]: https://nickel-lang.org
