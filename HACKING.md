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

### Happy-path testing

Tests for the happy path - i.e., valid Nickel programs which do not raise errors
are generally written in standalone Nickel files in the `tests/integration/pass`
directory. All `.ncl` files in this directory are automatically converted into
Rust integration tests, which run the file and assert that no errors were
raised during evaluation.

Each of these `.ncl` files is structured as an array of `Bool` expressions, which
is ultimately passed to a `check` function defined in
`tests/integration/pass/lib/assert.ncl`. This function applies an `Assert` contract
to each value in the array, which checks that the value it is applied to evaluates
to `true`. The benefit of using a contract for this is that if a test fails we
can simply run the file directly using Nickel, which gives better error messages
than the ones we get by default from `cargo test`.

### Testing failures

Tests which are expected to fail are predominantly written in Rust, because that
makes it possible to additionally check which particular error was raised. For
example, ([`tests/integration/records_fail.rs`](./tests/integration/records_fail.rs)):

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

The exception to the above is the suite of tests which check that the programs in
`examples/` behave as expected. These tests are also auto-generated from the example
programs, but since not every example is expected to pass, these files are annotated
with a comment declaring the expected behaviour. For example
([`examples/simple-contracts/simple-contract-div.ncl`](./examples/simple-contracts/simple-contract-div.ncl)):

```nickel
# test: blame

let Even = fun label value =>
  if builtin.is_number value && value % 2 == 0 then
    value
  else
    contract.blame label in
let DivBy3 = fun label value =>
  if builtin.is_number value && value % 3 == 0 then
    value
  else
    contract.blame label in
# Will cause an error! 4 is not divisible by 3.
(4 | Even
   | DivBy3)
```

### Snapshot testing

The project also contains a suite of snapshot tests in the `tests/snapshot`
directory. Here, `.ncl` files written in the subdirectories of the `input`
directory are run against the last-built Nickel binary, and their output is
compared to the last-known output.

Failures of these tests do not necessarily mean that anything is wrong. Rather
it should be seen as an opportunity to review the diffs and either accept
any changes, or fix any issues introduced.

See the [README] in the snapshot testing crate for more detailed guides on
working with snapshot tests.

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
