# The Nickel developer guide

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

- `nickel-lang` (path: `.`). The main crate containing the interpreter as a
  library as well as the `nickel` binary.
- `nickel-lang-lsp` (path: `lsp/nls/`). the Nickel Language Server (NLS), an LSP
  server for Nickel.
- `nickel-lang-utilities`: (path: `utilities/`). An auxiliary crate regrouping
   helpers for tests and benchmarks. Not required to build `nickel` itself.

Other noteworthy items:

- The user manual in `doc/manual/`, as a bunch of markdown files.
- A VSCode extension for NLS in `lsp/client-extension/`.

## Setup a development environment

### Using Nix

To set up a development environment using a recent Nix (>= 2.4):

1. Clone the repository: `git clone git@github.com:tweag/nickel.git`
2. At the root of the repository, run `nix develop`. You should now be dropped
   in a shell with all the required tool to hack on Nickel (`rust`, `cargo`,
   etc.)

### Without Nix

Otherwise, you can install the Rust toolchain separately: follow the
instructions of the [Rust installation guide][install-rust].

## Building

You can build all crates at once:

```shell
$ cargo build --all
$ ./target/debug/nickel --version
nickel-lang 0.1.0
$ ./target/debug/nls --version
nickel-lang-lsp 0.1.0
```

### Nickel

To only build the main crate `nickel-lang`, run:

```shell
$ cargo build
$ ./target/debug/nickel --version
nickel-lang 0.1.0
```

### NLS (nickel-lang-lsp)

To build NLS separately, the LSP server for Nickel, build the `nickel-lang-lsp` crate:

```shell
$ cargo build -p nickel-lang-lsp
$ ./target/debug/nls --version
nickel-lang-lsp 0.1.0
```

(Alternatively, you can run `cargo build` directly inside `lsp/nls/`).

### WebAssembly REPL

There is a WebAssembly (WASM) version of the REPL, which is used for the online
playground on [nickel-lang.org][nickel-lang.org]. To ease the build, we use the
`nickel-repl` located in `nickel-wasm-repl`, which just wraps and re-export
the `nickel-lang` with the right settings for building to WebAssembly.

The Nix flake has also an output to do the whole build, but incremental
compilation is not as good as with direct usage of `cargo`.

Both methods are described below.

#### Using Nix

At the root of the repository:

```shell
$ nix build .#nickelWasm
$ ls result/nickel-repl
LICENSE  package.json nickel_lang_bg.js  nickel_lang_bg.wasm [..]
```

#### Using Cargo

1. [Install `wasm-pack`][install-wasm-pack]
2. Run `wasm-pack` on the `nickel-repl` crate:

   ```shell
   cd nickel-repl-wasm
   wasm-pack build -- --no-default-features --features repl-wasm
   ```

   A `pkg` directory, containing the corresponding NPM package, should now be
   available.

## Testing

Tests are run via `cargo test`. They are two types of tests:

- Unit tests, located directly in the corresponding module.
- Integration tests, located in the dedicated crate `tests/integration`.

You can take inspiration from the existing tests to add your own. By convention,
tests expected to pass are written in a standalone Nickel file in
`tests/integration/pass/`. Each `.ncl` file defines a list of expressions that
must individually evaluate to the boolean `true`. The whole file is an
expression that returns true if and only if every tests pass, or fail with a
contract failure to help locating the failing test (instead of returning just
`false`).

If a test expected to pass is failing, run it directly using nickel with
`nickel -f tests/integration/pass/test_that_doesnt_pass.ncl` to get better error
messages than `cargo test`.

Tests expected to fail are often embedded directly into rust source code,
because you usually want to additionally check that the error is the one you
expect. For example ([`tests/integration/records_fail.rs`](./tests/records_fail.rs)):

```rust
#[test]
fn non_mergeable() {
    assert_matches!(
        eval("({a=1} & {a=2}).a"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
    assert_matches!(
        eval("({a | default = false} & {a | default = true}).a"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
}
```

## Benchmarking

If your change is likely to impact performance, it is recommended to run the
benchmark suite on master and on your branch to assess any performance changes.
Please report your findings in the description of the PR.

The benchmark suite is located in the `benches/` directory. To run it:

```shell
cargo bench
```

Note that a full run takes some time, up to a dozen of minutes. You can run
specific benchmarks instead of the full suite. Please refer to the documentation
of [`cargo bench`][doc-cargo-bench].

[nix]: https://nixos.org/
[install-rust]: https://www.rust-lang.org/tools/install
[install-wasm-pack]: https://rustwasm.github.io/wasm-pack/installer/
[doc-cargo-bench]: https://doc.rust-lang.org/cargo/commands/cargo-bench.html
[nickel-lang.org]: https://nickel-lang.org
