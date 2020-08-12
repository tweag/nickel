Nickel - Cheap configuration language
=====================================

Nickel is a lightweight configuration language. Its purpose is to automate the
generation of static configuration files - think JSON, YAML, XML, or your
favorite data representation language - that are then fed to another system. It
is designed to have a simple, well-understood core: at its heart, it is JSON
with functions. It adds other features on top of it to improve expressivity and
modularity, but you can do just fine without using it.

Nickel's important traits are:

- **Lightweight**: Nickel aims at being embeddable in a bigger software. As
    such, a simple and lightweight minimal interpreter should be reasonably
    simple to implement.  The reference interpreter should also be easily
    callable from various programming languages.
- **Functional**: the basic building blocks are functions. They are first-class
    citizens, which can be passed around, called and composed.
- **Gradual typing**: static types improve code quality, serve as a
    documentation and eliminate bugs early. On one hand, code specific to a
    particular configuration does not depend on external inputs and will always
    be evaluated to the same value, thus any type error will show up at run time
    anyway. Also, some JSON can be hard to type.  There, types are only a
    burden.  On the other hand, reusable code - that is, *functions* - is
    evaluated on potentially infinitely many different inputs, and is impossible
    to test exhaustively: there, types are precious.  Nickel has types, but you
    get to chose when you want it or not, and it handles safely the interaction
    between the typed and the untyped world.
- **Contracts**: complementary to the type system, contracts are a principled
    approach to dynamic type checking. They are used internally by the
    interpreter to insert guards at the boundary between typed and untyped
    chunks.  Nickel makes them available to the programmer as well, to give them
    the ability to enforce type assertions at runtime in a simple way.
- **Merge system**: while the basic computational blocks are functions, the
    basic data blocks are records (called objects in JSON). Nickel features a
    merge operation which lets you combine together such records modularly, but
    also to specify meta-data about the content of these records (documentation,
    default values, type contracts, etc.), called *enriched values*.

The motto guiding Nickel's design is:
> Great defaults, design for extensibility

There should be a standard, clear path for doing usual things. There should not
be arbitrary restrictions which limit you this one day you need to go beyond
usual to solve a hard specific problem.

## Use cases

Nickel should fit any situation when you need to generate a complex
configuration, be it for a software, a machine, a whole infrastructure, or a
build system.

The motivating use cases are in particular:
- The [Nix package manager](https://nixos.org/): Nix is a declarative package
    manager using its own language for specifying packages. Nickel is inspired
    in part by the Nix language, while trying to overcome some of its
    limitation. It could be used instead of the Nix language.
- (Cloud) infrastructure as code: infrastructure is becoming increasingly
    complex, requiring a rigorous approach to deployment, modification and
    configuration. This is where a declarative approach also shines, as adopted
    by [Terraform](https://www.terraform.io/),
    [NixOps](https://github.com/NixOS/nixops) or
    [Kubernetes](https://kubernetes.io/), all requiring potentially complex
    generation of configuration.
- Build systems: build systems are yet another piece of software
    which needs to dynamically generate configuration, the dependency graph for
    example. [Bazel](https://bazel.build/) rules may require a powerful
    language.

Several aforementioned projects have their own dedicated configuration language.
See the Related project section for a partial comparison. In general, such
specific languages may suffer from feature creep, lack of abstractions or just
feel ad-hoc.  Some are also totally fine but have just made different design
decisions and trade-offs.

## Getting started

### Build

1. Clone the repository in a local folder:
  ```
  $ git clone git@github.com:tweag/nickel.git
  $ cd nickel
  nickel$
  ```

2. Install build dependencies:
    - **With Nix**: If you have Nix installed, you can just type
    ```
    nickel$ nix-shell shell.nix
    [nix-shell:/tmp/nickel]$
    ```
    to be dropped in a shell, ready to build.

    - **Without Nix**: Otherwise, follow [this
      guide](https://doc.rust-lang.org/cargo/getting-started/installation.html)
      to install Rust and Cargo first.

3. Build Nickel:
  ```
  nickel$ cargo build
  ```
  And voilà ! Generated files are placed in `target/debug`.

### Run

1. (*optional*) Make a symbolic link to the executable:
  ```
  nickel$ ln -S nickel target/debug/nickel
  ```

2. Run your first program:
  ```
  nickel$ ./nickel <<< 'let x = 2 in x + x'
  Typechecked: Ok(Types(Dyn))
  Done: Num(4.0)
  ```
  Or load it from a file:
  ```
  nickel$ echo 'let s = "world" in "Hello, " ++ s' > program.ncl
  nickel$ ./nickel < program.ncl
  Typechecked: Ok(Types(Dyn))
  Done: Str("Hello, world")
  ```

By default, Nickel reads from the standard input. It may change in the future.

### Tests
```
nickel$ cargo test
```

### Documentation
1. Build the doc:
  ```
  nickel$ cargo doc --no-deps
  ```

2. Open the file `target/doc/nickel/index.html` in your browser.

### Examples
You can find examples in
[`src/examples`](https://github.com/tweag/nickel/tree/master/src/examples). Note
that as the syntax is not yet fixed, and some basic helpers are missing, they
may seem a bit alien currently.

## Roadmap
The design is settled and implemented for the most part, but the final syntax
and others important practical aspects are still being debated. We aim to
transition from an experimental stage to a minimum viable product stage.  The
next points to deal with are:

- Imports
- [List comprehensions](https://github.com/tweag/nickel/issues/80)
- [Destructuring](https://github.com/tweag/nickel/issues/81)
- [String interpolation](https://github.com/tweag/nickel/issues/82)
- [Recursive records](https://github.com/tweag/nickel/issues/83)
- Syntax

## Related projects and inspirations
- [Cue](https://cuelang.org/) is a configuration language with a focus on data
    validation. It has an original constraint system backed by a solid theory
    which ensures strong guarantees about your code. It allows for very elegant
    schema specifications. In return, the cost to pay is to abandon functions
    and
    [Turing-completeness](https://en.wikipedia.org/wiki/Turing_completeness).
    Nickel's merge system is inspired by the one of CUE, even if since Nickel
    does have general functions and is Turing-complete, they are necessarily
    different.
- [Nix](https://nixos.org/): The Nix language, or *Nix expressions*, is one of
    the main inspiration for Nickel. It is a very simple yet powerful lazy
    functional language. We strive to retain this simplicity, while adding
    typing capabilities, modularity, and detaching the language from the Nix
    package manager.
- [Dhall](https://dhall-lang.org/) is a statically typed configuration language.
    It is also inspired by Nix, to which it adds a powerful static type system.
    However, this forces the programmer to annotate all of their code with types.
- [Jsonnet](https://jsonnet.org/) is another language which could be dubbed as
    "JSON with functions" (and others things as well). It is a lazy functional
    language with object oriented features, among which inheritance is similar
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
comparison with a selection of these languages.

## License
[MIT License](https://github.com/tweag/nickel/blob/master/LICENSE).

Copyright (c) Tweag Holding and its affiliates.
