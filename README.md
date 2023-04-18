# Nickel

[![Continuous integration](https://github.com/tweag/nickel/workflows/Continuous%20integration/badge.svg)](https://github.com/tweag/nickel/actions?query=branch%3Amaster)
[![Website](https://img.shields.io/website-up-down-green-red/http/cv.lbesson.qc.to.svg)](https://nickel-lang.org)

Nickel is the cheap configuration language.

Its purpose is to automate the generation of static configuration files - think
JSON, YAML, XML, or your favorite data representation language - that are then
fed to another system. It is designed to have a simple, well-understood core: it
is in essence JSON with functions.

Nickel's salient traits are:

- **Lightweight**: Nickel is easy to embed. An interpreter should be simple to
    implement. The reference interpreter can be called from many programming
    languages.
- **Composable code**: the basic building blocks for computing are functions.
    They are first-class citizens, which can be passed around, called and
    composed.
- **Composable data**: the basic building blocks for data are records
    (called *objects* in JSON). In Nickel, records can be merged at will,
    including associated metadata (documentation, default values, type
    contracts, etc).
- **Typed, but only when it helps**: static types improve code quality, serve as
    documentation and eliminate bugs early. But application-specific
    self-contained code will always evaluate to the same value, so type errors
    will show up at runtime anyway. Some JSON is hard to type. There, types are
    only a burden. Whereas reusable code - that is, *functions* - is evaluated
    on potentially infinitely many different inputs, and is impossible to test
    exhaustively. There, types are precious. Nickel has types, but you get to
    choose when you want it or not, and it handles safely the interaction between
    the typed and the untyped world.
- **Design by contract**: complementary to the type system, contracts are
    a principled approach to checking assertions. The interpreter automatically
    inserts assertions at the boundary between typed and untyped code. Nickel
    lets users add arbitrary assertions of their own and easily understand why
    when assertions fail.

The motto guiding Nickel's design is:
> Great defaults, design for extensibility

There should be a standard, clear path for common things. There should be no
arbitrary restrictions that limit what you can do you the one day you need to go
beyond.

## Use cases

Nickel is a good fit in any situation where you need to generate a complex
configuration, be it for a single app, a machine, whole infrastructure, or a
build system.

The motivating use cases are in particular:

- The [Nix package manager](https://nixos.org/): Nix is a declarative package
    manager using its own language for specifying packages. Nickel is an
    evolution of the Nix language, while trying to overcome some of its
    limitations.
- Infrastructure as code: infrastructure is becoming increasingly complex,
    requiring a rigorous approach to deployment, modification and configuration.
    This is where a declarative approach also shines, as adopted by
    [Terraform](https://www.terraform.io/),
    [NixOps](https://github.com/NixOS/nixops) or
    [Kubernetes](https://kubernetes.io/), all requiring potentially complex
    generation of configuration.
- Build systems: build systems (like [Bazel](https://bazel.build/)) need
    a specification of the dependency graph.

Most aforementioned projects have their own bespoke configuration language. See
[Comparison](#comparison). In
general, application-specific languages might suffer from feature creep, lack of
abstractions or just feel ad hoc. Nickel buys you more for less.

## Getting started

Please follow the getting started guide for Nickel users on the [nickel-lang
website](https://nickel-lang.org/getting-started). The instructions below are
either reproduced for this document to be self-contained or because
they are aimed toward hacking on the Nickel interpreter itself (e.g. building
the `nickel-lang` crate documentation).

### Run

1. Get a Nickel binary:
   - With [flake-enabled](https://nixos.wiki/wiki/Flakes) Nix, run
     Nickel directly with `nix run github:tweag/nickel`. You can use [our binary
     cache](https://tweag-nickel.cachix.org/) to prevent rebuilding a lot of
     packages. Pass arguments to Nickel with an extra `--` as in `nix run
     github:tweag/nickel -- repl`,
   - Again with flake-enabled Nix, you can install Nickel in your profile with
     `nix profile add github:tweag/nickel`. The `nickel` command is then in your
     `$PATH` and is available anywhere.
   - Without Nix, you can use `cargo run` after [building](#build), passing
     arguments with an extra `--` as in `cargo run -- -f program.ncl`.

2. Run your first program:

    ```console
    $ nickel <<< 'let x = 2 in x + x'
    4
    ```

    Or load it from a file:

    ```console
    $ echo 'let s = "world" in "Hello, " ++ s' > program.ncl
    $ nickel -f program.ncl
    "Hello, world"
    ```

3. Start a REPL:

    ```console
    $ nickel repl
    nickel> let x = 2 in x + x
    4

    nickel>
    ```

    Use `:help` for a list of available commands.
4. Export your configuration to JSON, YAML or TOML:

  ```console
  $ nickel export --format json <<< '{foo = "Hello, world!"}'
  {
    "foo": "Hello, world!"
  }
  ```

Use `nickel help` for a list of subcommands, and `nickel help <subcommand>`
for help about a specific subcommand.

#### Editor Setup

Nickel has syntax highlighting plugins for Vim/Neovim, and VSCode. In-editor
diagnostics, type hints, and auto-completion are provided by the Nickel Language
Server. Please follow
[the LSP guide](https://github.com/tweag/nickel/tree/master/lsp) to set up syntax
highlighting and NLS.

#### Formatting

You can format Nickel source code using [Topiary](https://github.com/tweag/topiary/):

```console
topiary -i -f my-config.ncl
```

Please follow the Formatting Capabilities section of the [LSP
documentation](https://github.com/tweag/nickel/tree/master/lsp) to know how to
hook up the Nickel LSP and topiary in order to enable formatting inside your
code editor.

### Build

[rust-guide]: https://doc.rust-lang.org/cargo/getting-started/installation.html

1. Download build dependencies:

   - **With Nix**: If you have [Nix](https://nixos.org/nix) installed:

     ```console
     nix-shell
     nix develop # if you use Nix Flakes
     ```

     You will be dropped in a shell, ready to build. You can use
     [our binary cache](https://tweag-nickel.cachix.org/) to prevent rebuilding
     a lot of packages.
   - **Without Nix**: otherwise, follow [this guide][rust-guide] to install Rust
     and Cargo first.

1. Build Nickel:

   ```console
   cargo build --release
   ```

   And voilà! Generated files are placed in `target/release`.

### Test

Run tests with

```console
cargo test
```

### Documentation

The user manual is available [on the nickel-lang.org
website](https://nickel-lang.org/user-manual/introduction), and in this
repository as a collection of Markdown files in `doc/manual`.

To get the documentation of the `nickel-lang` codebase itself:

1. Build the doc:

    ```console
    cargo doc --no-deps
    ```

2. Open the file `target/doc/nickel/index.html` in your browser.

### Examples

You can find examples in the [`./examples`](./examples) directory.

## Current state and roadmap

Nickel is currently released in version `0.3.1`. This version should be
functional and is intended to gather feedback and real-life testing. The next
planned version is `1.0`. The next steps we plan to work on are:

- Nix integration: being able to seamlessly use Nickel to write packages and
  shells ([nickel-nix](https://github.com/nickel-lang/nickel-nix))
- Custom merge functions (second part of the
  [overriding
  proposal](https://github.com/tweag/nickel/blob/9fd6e436c0db8f101d4eb26cf97c4993357a7c38/rfcs/001-overriding.md))
- Incremental evaluation: design an incremental evaluation model and a caching
  mechanism in order to perform fast re-evaluation upon small changes to a
  configuration.
- Performance improvements

## Comparison

- [CUE](https://cuelang.org/) is a configuration language with a focus on data
    validation. It introduces a new constraint system backed by a solid theory
    which ensures strong guarantees about your code. It allows for very elegant
    schema specifications. In return, the cost to pay is to abandon functions
    and
    [Turing-completeness](https://en.wikipedia.org/wiki/Turing_completeness).
    Nickel's merge system is inspired by the one of CUE, even if since Nickel
    does have general functions and is Turing-complete, they are necessarily
    different.
- [Nix](https://nixos.org/): The Nix language, or *Nix expressions*, is one of
    the main inspirations for Nickel. It is a very simple yet powerful lazy
    functional language. We strive to retain this simplicity, while adding
    typing capabilities, modularity, and detaching the language from the Nix
    package manager.
- [Dhall](https://dhall-lang.org/) is a statically typed configuration language.
    It is also inspired by Nix, to which it adds a powerful static type system.
    However, this forces the programmer to annotate all of their code with types.
- [Jsonnet](https://jsonnet.org/) is another language which could be dubbed as
    "JSON with functions" (and others things as well). It is a lazy functional
    language with object-oriented features, among which inheritance is similar
    to Nickel's merge system. One big difference with Nickel is the absence of
    typing.
- [Pulumi](https://www.pulumi.com/) is not a language in itself, but a cloud
    tool (like Terraform) where you can use your preferred language for
    describing your infrastructure. This is a different approach to the problem,
    with different trade-offs.
- [Starlark](https://docs.bazel.build/versions/master/skylark/language.html) is
    the language of [Bazel](https://bazel.build/), which is a dialect of
    [Python](https://www.python.org/). It does not have types and recursion is
    forbidden, making it not Turing-complete.

See [RATIONALE.md](./RATIONALE.md) for the design rationale and a more detailed
comparison with these languages.

### Comparison with other configuration languages
<!-- Intentionally duplicated in `RATIONALE.md`, please update the other one for
any change done here -->

| Language | Typing                        | Recursion  | Evaluation | Side-effects                                     |
|----------|-------------------------------|------------|------------|--------------------------------------------------|
| Nickel   | Gradual (dynamic + static)    | Yes        | Lazy       | Yes (constrained, planned)                       |
| Starlark | Dynamic                       | No         | Strict     | No                                               |
| Nix      | Dynamic                       | Yes        | Lazy       | Predefined and specialized to package management |
| Dhall    | Static (requires annotations) | Restricted | Lazy       | No                                               |
| CUE      | Static (everything is a type) | No         | Lazy       | No, but allowed in the separated scripting layer |
| Jsonnet  | Dynamic                       | Yes        | Lazy       | No                                               |
| JSON     | None                          | No         | Strict     | No                                               |
| YAML     | None                          | No         | N/A        | No                                               |
| TOML     | None                          | No         | N/A        | No                                               |
