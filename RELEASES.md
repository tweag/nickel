Version 0.2  (2022-07-XX)
=========================

Various bug fixes, language and tooling improvements.

Breaking changes
----------------

- Using a contract as part of a static type annotations will in most cases fail
  with an appropriate error message. This is a temporary limitation in order to
  fix previously unsound behavior in the typechecker. This will likely be fixed
  in the upcoming 0.3.x release. For more details, see issues
  [#701](https://github.com/tweag/nickel/issues/701) and
  [#724](https://github.com/tweag/nickel/issues/724)

Fixes
-----

- Fix unnecessarily restricted record contract for `record.update`
- Fix wrong interpretation of long interpolation-like sequences `%..%{` in strings
- Fix panic when evaluating a `switch` in specific cases
- Fix fields without definition being assigned to `null`, instead of just being
  marked as undefined

Language features
-----------------

- Merge null values together give null, and merging empty lists together gives
  an empty list, instead of failing with a `error: non mergeable terms`
- Add recursive let-bindings (`let rec`)
- Add type wildcard. Use `_` in place of a type to let the typechecker fill
  the gap. Example: `let foo : _ = array.all ((==) 2) [1,2,3]`
- Add `builtin.to_str` and `string.from` to convert generic values to a string

Tooling
-------

- Add the `nickel pprint-ast` command to pretty print a parsed program (for
  debugging purpose)
- Add the `nickel doc` command to produce markdown documentation from the
  in-code `doc` metadata

Documentation
-------------

- Fix various typos and use of deprecated syntax in the user manual

Version 0.1  (2022-03-10)
=========================

First release! The main focus has been the design of the Nickel language itself.

Language features
-----------------

- Gradual type system with row types, polymorphism and type inference
- Contract system for data validation
- Merge system for recursive records that supports one level of overriding
- Metadata annotations (default values, documentation, etc.)
- Unified syntax for terms, types and contracts (RFC002)
- Record destructuring

Tooling
-------

- The main binary supports the following subcommands:
  - `nickel query` to show metadata and documentation of library functions,
     the field of a configuration, etc.
  - `nickel export` to serialize to JSON, YAML, or TOML
  - `nickel repl` to launch an REPL
  - `nickel typecheck` to do typechecking without evaluating

- An LSP-server is included

Documentation
-------------

- User manual sections on syntax, correctness (types and contracts), and
  merging
- The standard library has been documented in-code (use
     `nickel query`/`:query` to retrieve it)

Known limitations
-----------------

- The roadmap for overriding and the merge system (RFC001) has not been
  implemented fully yet.

- Performance has not been prioritized.

- Due to the use of reference counting as a memory management strategy,
  mutually recursive record fields are currently leaking memory. This
  shouldn't be an issue in a standard workflow.

- Standard library APIs and language features are subject to change. There
  is no backward compatibility guarantees for this version. In general, this
  release is meant for experimenting and getting user feedback, but isn't
  intended to be used in production.
