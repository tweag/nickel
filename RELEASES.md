Version 1.2
===========

Version 1.2 comes with several improvements on the LSP and other components of
the Nickel tooling. The new customize mode of the CLI makes it possible to
dynamically turn a configuration into a command-line interface, which you can
interact with.

Several related long-standing issues and limitations when typechecking
polymorphic functions are also finally fixed.

Core language
-------------

* Make the lexer accept scientific notation by @vkleen in https://github.com/tweag/nickel/pull/1456
* Improve polymorphism handling, bidirectional typechecking and fix unsound generalization by @yannham in https://github.com/tweag/nickel/pull/1372
* Error on serializing very large numbers by @vkleen in https://github.com/tweag/nickel/pull/1470
* Import YAML files containing multiple documents as arrays by @vkleen in https://github.com/tweag/nickel/pull/1497
* Normalize line endings in string literals during parsing (Windows compatiblity) by @vkleen in https://github.com/tweag/nickel/pull/1562
* Implement curried dot operator by @suimong in https://github.com/tweag/nickel/pull/1578
* Statically merge partial record definitions by @vkleen in https://github.com/tweag/nickel/pull/1599
* Disable recursive priorities by @yannham in https://github.com/tweag/nickel/pull/1600

Fixes
-----

* Various fixes to the pretty-printer by @vkleen (https://github.com/tweag/nickel/pull/1411, https://github.com/tweag/nickel/pull/1412, https://github.com/tweag/nickel/pull/1410, https://github.com/tweag/nickel/pull/1417)
* Fix REPL panic on transitive imports by @Radvendii in https://github.com/tweag/nickel/pull/1474
* Fix contract application order in let bindings and annotations by @vkleen in https://github.com/tweag/nickel/pull/1544
* Fix cursor desyncing when using REPL by @deotimedev in https://github.com/tweag/nickel/pull/1546
* Fix handling of relative imports @jneem in https://github.com/tweag/nickel/pull/1489
* LSP Fix two issues with incomplete input by @jneem in https://github.com/tweag/nickel/pull/1550
* Fix incomplete check of record row constraints by @yannham in https://github.com/tweag/nickel/pull/1558
* Fix LSP regression on hovering by @yannham in https://github.com/tweag/nickel/pull/1583

Tooling
-------

* Full vscode extension by @szlend and @yannham in (https://github.com/tweag/nickel/pull/1405, https://github.com/tweag/nickel/pull/1413, https://github.com/tweag/nickel/pull/1416)
* Invalidate importers in NLS (plus a couple other import-related issues) by @jneem in https://github.com/tweag/nickel/pull/1426
* Improve `nickel query` (and `:query` in the REPL) interface by @yannham in
  https://github.com/tweag/nickel/pull/1447
* Topiary integration (formatting) as `nickel format` by @vkleen in https://github.com/tweag/nickel/pull/1371
* LSP: various improvement to completion by @jneem (https://github.com/tweag/nickel/pull/1450, https://github.com/tweag/nickel/pull/1473)
* Improve doc extraction capabilities through `nickel doc` (evaluate terms before extracting documentation) by @vkleen in https://github.com/tweag/nickel/pull/1463
* LSP Fix persistent diagnostics by @jneem in https://github.com/tweag/nickel/pull/1478
* Restore --version for CLI, include git revision by @yannham in https://github.com/tweag/nickel/pull/1486
* Disable the colors when stdout isn't a tty by @thufschmitt in https://github.com/tweag/nickel/pull/1508
* Improve type variables name allocation when reporting type errors by @yannham in https://github.com/tweag/nickel/pull/1512
* LSP formatting without calling the topiary binary by @vkleen in https://github.com/tweag/nickel/pull/1526
* LSP Initial handling of incomplete input by @jneem in https://github.com/tweag/nickel/pull/1541
* LSP resolve imports for incomplete inputs also by @jneem in https://github.com/tweag/nickel/pull/1542
* Add multiline string support for VSCode autoclosing pairs by @deotimedev in https://github.com/tweag/nickel/pull/1553
* Display icon for nickel file in vscode explorer by @suimong in https://github.com/tweag/nickel/pull/1556
* New experimental customize mode CLI to interact with a configuration on the
  command line (see `nickel help export`) by @Radvendii and @yannham in
  https://github.com/tweag/nickel/pull/1475
* (LSP) Import completion by @deotimedev in https://github.com/tweag/nickel/pull/1561
* Get rid of uninformative generic notes for higher-order contract errors by @yannham in https://github.com/tweag/nickel/pull/1564
* Render hover documentation as Markdown in LSP by @deotimedev in https://github.com/tweag/nickel/pull/1559
* Exit with nonzero exit code on failure by @vkleen in https://github.com/tweag/nickel/pull/1576

Stdlib
------

* add `std.contract.Sequence` by @Radvendii in https://github.com/tweag/nickel/pull/1452

Documentation
-------------

* manual docs: fix mismatch between input command and its echo in the output by @foo-jin in https://github.com/tweag/nickel/pull/1421
* Add two examples: imports and foreach pattern by @mipmip in https://github.com/tweag/nickel/pull/1387
* Fix some leftover instances of the old enum syntax in the manual by @vkleen in https://github.com/tweag/nickel/pull/1548

Version 1.1
===========

This version mostly includes bugfixes and stdlib improvement since 1.0.

**IMPORTANT**: The main crate `nickel-lang` has been split between
`nickel-lang-cli` (the binary) and `nickel-lang-core` (the library). If you're
using `cargo` to install Nickel, please uninstall the previous crate by running
`cargo uninstall nickel-lang`, and from now one use `cargo install
nickel-lang-cli` to install 1.1 and do further updates.

Stdlib
------

- Encode more pre-conditions of stdlib functions as additional contracts,
  replacing dynamic type errors with nice contract errors by @yannham in https://github.com/tweag/nickel/pull/1358
- ArraySliceFun incorrectly excluded the length of the array as the end index by @vkleen in https://github.com/tweag/nickel/pull/1396

Tooling
-------

- Improve the pretty printing of terms in the CLI and within error messages by @vkleen in https://github.com/tweag/nickel/pull/1262
- Fix LSP panic when importing JSON by @yannham in https://github.com/tweag/nickel/pull/1382
- Fix LSP hanging under certain conditions involving external imports by @yannham in https://github.com/tweag/nickel/pull/1390
- Better error message when contract makes certain rows illegal by @Radvendii in https://github.com/tweag/nickel/pull/1323
- Fix function params hovering in the LSP by @yannham in https://github.com/tweag/nickel/pull/1395
- Fix LSP infinite loop on hovering on external imports by @yannham in https://github.com/tweag/nickel/pull/1397

Fixes
-----

- Fixes incorrect variable names in type mismatch errors by @vkleen in https://github.com/tweag/nickel/pull/1313
- Improve error messages for polymorphic tail parametricity violations by @matthew-healy in https://github.com/tweag/nickel/pull/1320
- Fix panic on duplicated top-level idents in record destructuring by @matthew-healy in https://github.com/tweag/nickel/pull/1324
- Prevent panic when parsing record type with field path by @matthew-healy in https://github.com/tweag/nickel/pull/1325
- Give a better error message when trying to query a non-record by @jneem in https://github.com/tweag/nickel/pull/1326
- Fix error position by @jneem in https://github.com/tweag/nickel/pull/1333
- Fix panic when interpolating fields in a record type by @jneem in https://github.com/tweag/nickel/pull/1332
- Fix type annotations not generating contracts by @yannham in https://github.com/tweag/nickel/pull/1379
- Fix typechecker looping by adding missing check for unifying equal type vars by @yannham in https://github.com/tweag/nickel/pull/1400
- Fix panic on function contract failure (position of arrow functions not set) by @yannham in https://github.com/tweag/nickel/pull/1407

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
