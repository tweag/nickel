---
feature: stable public API
start-date: 2025-09-05
author: Joe Neeman
---

# A stable API for Nickel

Currently, the only stable interface to Nickel is the CLI tool; anyone
wanting to embed the Nickel interpreter needs to deal with the
`nickel_lang_core` crate, which changes frequently and isn't
particularly user-friendly.

This RFC proposes a simple Nickel API. Its main goals are

- Simplicity: it should be easy to do basic things, like evaluating
  some Nickel code into a structured representation.
- Stability: we aim not to break public APIs.
- Embeddability: our interfaces should be usable from
   other languages.

## Expressions

We want to avoid exposing our AST and runtime representation, because
they are subject to change. However, we certainly want to allow for
inspecting the basic Nickel value types: booleans, numbers, strings,
enums, records, and arrays. We also want to allow for some control
over evaluation, so we should allow some kind of thunk.

We will define an opaque `Expr` struct representing one of the value
types above, with methods for querying the kind and extracting values.
Our initial version will omit thunks.

Access to arrays will be done by querying the length and indexing.
(The Rust interface could also have an iterator, but the C FFI will
need the simpler interface.) Access to records can be done by name,
or by querying the length and indexing (again, the Rust interface
could also have an iterator). Records can also support access to
field metadata.

In the C FFI, `Expr` will be represented by an opaque pointer type
(say, `nickel_expr_t`).
The data behind the pointer will be owned on the Rust side, and there
will be a `nickel_expr_free` function. Strings will be
returned to C as a `nickel_string_t` struct containing a pointer
and a length; there will be a `nickel_string_free` function.
When you look up an element in an array or record, it will return an
owned `nickel_expr_t` that you are responsible for freeing.

### Alternative: a non-exhaustive enum

Instead of having `Expr` as an opaque struct, it could be a non-exhaustive
enum. This might be nicer for Rust consumers, but anyway we'd need
to turn it into an opaque pointer for C FFI.

## Expression serialization and conversion

In principle, querying structured data from expressions is all the
user needs. In practice, Nickel already has some more convenient
ways to extract data and so we should give access to them.

- We should allow conversion from `Expr` to JSON, YAML, and TOML.
  These will return a single, large string. In the future, there
  could also be a lazier version with a `std::io::Write`-like
  interface.
- We should allow conversion from `Expr` to serde-compatible
  rust types (and maybe facet-compatible?).

## Error messages

We should allow for embedders of Nickel to output the same error messages
as the Nickel interpreter. That means we need an opaque `Error` type,
with methods for formatting errors (in structured or text format, possibly
with terminal-compatible colors).

## Taking input

We need to allow for accepting input and turning it into an `Expr`.
We should allow the input to be Nickel code, or any format importable
by Nickel (JSON, YAML, etc.). Ideally (but maybe not initially),
this part should be as configurable as the Nickel CLI. For example,
it should allow for configuration of input paths and variable overrides.

## Evaluation

We should have a function for full evaluation, where the resulting
expression will have no thunks. We should have a function (but maybe
not in the initial version) for evaluation to WHNF. We should have
a function for typechecking input without evaluating it. *Maybe* we should
have functions for more specialized kinds of evaluation.
