Version 1.0
===========

This is the 1.0 release! The syntax and the semantics of the core language have
been stabilized and shouldn't evolve as much in the future.

- The core semantics of the language have been reworked and stabilized, in
  particular (but not limited to) merging, types and contracts with the
  implementation of RFC005
- The stdlib has been augmented with many new functions
- Parts of the syntax and some builtin symobls (types, stdlib functions, and so
  on) have been improved and made consistent
- New features for the LSP, and in particular code completion

Breaking changes
----------------

- Various functions from the stdlib have been renamed for better discoverability,
    and the stdlib got a lot of new additions. Please refer to the documentation
    of the stdlib.
- String functions are now unicode-aware, and operate on the Unicode grapheme
    cluster abstraction instead of the character abstraction (`string.length`,
    `string.is_match`, etc.)
- The `switch` keyword has been replaced by `match`, and can now be used as a
    standalone function (doesn't need to be applied right away)
- The `Num` and `Str` builtin types have been renamed to `Number` and `String` by @yannham in https://github.com/tweag/nickel/pull/1164
- The `num` and `str` stdlib modules have been renamed to `number` and `string`
- The `builtin.typeof` function now returns `'Number`, `'String`, `'Function`
  instead of respectively `'Num`, `'Str`, and `'Fun`
- The `builtin.is_num`, `builtin.is_str` and `builtin.to_str` functions have been
  renamed to `is_number`, `is_string` and `to_string`
- The `string.to_num` and `string.from_num` functions have been renamed to
  `to_number` and `from_number`
- All the stdlib modules `array`, `string`, `record`, etc. have been put
    under a `std` namespace. They must now be accessed as `std.array`,
    `std.string` and so on.
- RFC005 was implemented, which changes the semantics of contract annotations
    and merging. See [the RFC
    content](https://github.com/tweag/nickel/blob/master/rfcs/005-metadata-rework.md)
    for more details. Most notably, metadata annotation (default values,
    `optional`, documentation, etc.) can only appear next to a record field.
    Contract and type annotations can still appear anywhere. Documentation can
    still appear on let-bindings.
- Use static dictionary types for `record.fields` and `record.values` by @matthew-healy in https://github.com/tweag/nickel/pull/1024
- Make type annotations to not propagate through merging by @yannham in https://github.com/tweag/nickel/pull/1271
- Change to dictionary contracts and introduction of a separate dictionary contract (in addition to dictionary type):
  - Fix dictionary contract semantics by @vkleen in https://github.com/tweag/nickel/pull/1141
  - Introduce dictionary contracts by @yannham in https://github.com/tweag/nickel/pull/1272
- Stdlib `string.Stringingable` -> `string.Stringable` by @vkleen in https://github.com/tweag/nickel/pull/1180
- Fix the type of `array.elem` by @yannham in https://github.com/tweag/nickel/pull/1223
- Change the enum tag start delimiter from backtick to single-quote by @vkleen in https://github.com/tweag/nickel/pull/1279
- `import` is now a statement, `import "foo.ncl" arg1 arg2` requires parenthesis now: `(import "foo.ncl") arg1 arg2`, see https://github.com/tweag/nickel/pull/1293

Language features
-----------------

- Symbolic strings by @matthew-healy in https://github.com/tweag/nickel/pull/994
- [RFC005] Lazy propagation by @yannham in https://github.com/tweag/nickel/pull/1086
- Non-exported record fields by @vkleen in https://github.com/tweag/nickel/pull/1132
- Enrich label custom reporting data by @yannham in https://github.com/tweag/nickel/pull/1152
- Use type annotations in record patterns during typechecking by @matthew-healy in https://github.com/tweag/nickel/pull/1176
- Use arbitrary precision rationals as the underlying representation of numbers by @yannham in https://github.com/tweag/nickel/pull/1182
- Allow equal arrays to be merged in order to make merge idempotent by @yannham in https://github.com/tweag/nickel/pull/1229
- Use deterministic hashtables, making runtime errors and typechecking error deterministic by @yannham in https://github.com/tweag/nickel/pull/1235

Stdlib
------

- Add a %trace% primop (and `std.trace` function) by @vkleen in https://github.com/tweag/nickel/pull/1055
- Add `std.contract.Equal` contract to the stdlib by @yannham in https://github.com/tweag/nickel/pull/1203

Tooling
-------

- Display meta information when providing completion in https://github.com/tweag/nickel/pull/966
- LSP completion for import terms by @ebresafegaga in https://github.com/tweag/nickel/pull/993
- Fix LSP server not giving completion when a non-contract meta-information is present in a declaration by @ebresafegaga in https://github.com/tweag/nickel/pull/991
- Fix LSP not giving completion when an identifier is prefixed by a delimiting character by @ebresafegaga in https://github.com/tweag/nickel/pull/1043
- Improve the "goto definition" feature of the LSP by making it work across multiple files by @ebresafegaga in https://github.com/tweag/nickel/pull/1029
- Add --color option to the CLI by @matthew-healy in https://github.com/tweag/nickel/pull/1033
- Make the "find references" feature of the LSP work across multiple files by @ebresafegaga in https://github.com/tweag/nickel/pull/1037
- Add support for LSP completion using the surrounding context by @ebresafegaga (context completion) in https://github.com/tweag/nickel/pull/1057
- Support completion for field names inside recursive records. by @ebresafegaga in https://github.com/tweag/nickel/pull/1088
- Improve contract violation error reporting by @ebresafegaga in https://github.com/tweag/nickel/pull/1139
- Add a JSON documentation export option by @vkleen in https://github.com/tweag/nickel/pull/1209
- Add support for formatting capabilities to the LSP by @ebresafegaga in https://github.com/tweag/nickel/pull/1216

Fixes
-----

- Do not panic on type path mismatch by @yannham in https://github.com/tweag/nickel/pull/1028
- Fix multiline string interpolation when preceded by a `"` character by @matthew-healy in https://github.com/tweag/nickel/pull/1023
- Improve the performance of `std.array.fold_left` and `std.array.fold_right` by @vkleen in https://github.com/tweag/nickel/pull/1075
- Correctly type identifiers introduced in destructuring patterns by @matthew-healy in https://github.com/tweag/nickel/pull/1099
- Always include fields with a value in `record.fields` by @vkleen in https://github.com/tweag/nickel/pull/1225
- Make partially applied boolean operators work by @vkleen in https://github.com/tweag/nickel/pull/1282

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
