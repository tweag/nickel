Version 0.3.1 (2022-12-15)
==========================

Fixes
-----

- Fix blame error when calling `string.find` by @euank in https://github.com/tweag/nickel/pull/988

Version 0.3  (2022-12-07)
=========================

Fixes
-----

- Fix polymorphic contracts unduly changing semantics by @ebresafegaga in https://github.com/tweag/nickel/pull/802
- Fix typechecking and unification in presence of flat types (aka opaque types, aka contracts) by @yannham in https://github.com/tweag/nickel/pull/766
- Fix polarity for polymorphic contract failure by @ebresafegaga in https://github.com/tweag/nickel/pull/831
- Fix panic when a row mismatch occurs while unifying row tails by @matthew-healy in https://github.com/tweag/nickel/pull/847
- Fix type to term conversion causing unbound type variables errors by @francois-caddet in https://github.com/tweag/nickel/pull/854
- Fix bad lexing of enum tags by @matthew-healy in https://github.com/tweag/nickel/pull/874
- Fix multiple recursive overriding by @yannham in https://github.com/tweag/nickel/pull/940

Language features
-----------------

- Optional fields by @yannham in https://github.com/tweag/nickel/pull/815
- Numeral merge priorities by @yannham in https://github.com/tweag/nickel/pull/829
- Recursive merge priorities (or push-priorities, or leafy priorities) by @yannham in https://github.com/tweag/nickel/pull/845
- Change `switch` to `match` and make it a proper function by @yannham in https://github.com/tweag/nickel/pull/970

Stdlib
------

- Statically type `string.join` by @matthew-healy in https://github.com/tweag/nickel/pull/946

Tooling
-------

- Add record completion in the LSP by @ebresafegaga in
  - https://github.com/tweag/nickel/pull/867
  - https://github.com/tweag/nickel/pull/909
  - https://github.com/tweag/nickel/pull/913
  - https://github.com/tweag/nickel/pull/914
- Add completion for Nickel's stdlib in the LSP by @ebresafegaga in https://github.com/tweag/nickel/pull/918

Performances
------------

- Lazy array contracts by @fuzzypixelz in https://github.com/tweag/nickel/pull/809
- Array slices by @fuzzypixelz in https://github.com/tweag/nickel/pull/776
- String interning for identifiers by @Acaccia in https://github.com/tweag/nickel/pull/835

Version 0.2  (2022-07-29)
=========================

Breaking changes
----------------

- Using a contract as part of a static type annotation will in most cases fail
  with an appropriate error message. This is a temporary limitation in order to
  fix previously unsound behavior in the typechecker. This restriction will
  likely be lifted in the upcoming 0.3.x release. For more details, see issues
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

- Merging null values together gives null, and merging empty lists together gives
  an empty list, instead of failing with `error: non mergeable terms`
- Add recursive let-bindings (`let rec`)
- Add type wildcards. Use `_` in place of a type to let the typechecker fill
  the gap. Example: `let foo : _ = array.all ((==) 2) [1,2,3]`
- Add `builtin.to_str` and `string.from` to convert generic values to a string
- Re-introduce an official syntax for enum types

Tooling
-------

- Add the `nickel pprint-ast` command to pretty print a parsed program (mostly
  debugging purpose)
- Add the `nickel doc` command to produce markdown documentation from the
  in-code `doc` metadata

Documentation
-------------

- Fix various typos and remove use of deprecated syntax in the user manual

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
