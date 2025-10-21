Version 1.14 (2025-10-02)
=========================

Nickel 1.14 has no core language changes. It adds position information to
YAML documents, so that if some imported YAML breaks a contract then Nickel
can blame the correct location in the YAML file. Nickel's LSP has had some
bugfixes (thanks to @L0r3m1p5um), fixing a crash and avoiding stale data.

LSP
---

* Implement textDocument/didClose method for LSP by @L0r3m1p5um in https://github.com/tweag/nickel/pull/2323
* Pass sources from World into background eval by @L0r3m1p5um in https://github.com/tweag/nickel/pull/2338
* Avoid copying file contents in LSP eval by @L0r3m1p5um in https://github.com/tweag/nickel/pull/2347
* Fix crash in NLS when creating typechecking diagnostics by @L0r3m1p5um in https://github.com/tweag/nickel/pull/2357
* Fix performance of UsageLookup by @jneem in https://github.com/tweag/nickel/pull/2339

Tooling
-------

* Allow index packages to live in subdirectories of the repo by @jneem in https://github.com/tweag/nickel/pull/2315
* Convert to saphyr for positions in yaml by @jneem in https://github.com/tweag/nickel/pull/2332
* Initial public api rfc by @jneem in https://github.com/tweag/nickel/pull/2337
* Clean up Ident conversions to reduce accidental allocations by @jneem in https://github.com/tweag/nickel/pull/2341
* Declare MSRV as 1.85 by @brandonweeks in https://github.com/tweag/nickel/pull/2364
* Narrow serde_with features by @brandonweeks in https://github.com/tweag/nickel/pull/2366
* Bump malachite to version 0.6 by @brandonweeks in https://github.com/tweag/nickel/pull/2367
* Bump toml to version 0.9 and toml_edit to version 0.23 by @brandonweeks in https://github.com/tweag/nickel/pull/2368
* Plumb custom_transform into EntryState to minimize traversal by @twitchyliquid64 in https://github.com/tweag/nickel/pull/2346
* Restore the static nix release jobs by @jneem in https://github.com/tweag/nickel/pull/2373

Documentation
-------------

* Fix broken note and other cosmetic changes in pkg manual by @yannham in https://github.com/tweag/nickel/pull/2329

## New Contributors
* @brandonweeks made their first contribution in https://github.com/tweag/nickel/pull/2364
* @twitchyliquid64 made their first contribution in https://github.com/tweag/nickel/pull/2346

Version 1.13 (2025-08-18)
=========================

Nickel 1.13 includes various bug-fixes and some improvements to error messages,
but no core language changes. There has been substantial work towards a new
runtime representation, which will eventually reduce the interpreter's memory
usage substantially (and it is also a step towards the upcoming bytecode
interpreter).

LSP
---

* Fix cache miss when pulling source file by a relative path by @L0r3m1p5um in https://github.com/tweag/nickel/pull/2276
* Publish diagnostics for parsing errors in imported files by @L0r3m1p5um in https://github.com/tweag/nickel/pull/2299

Tooling
-------

* Wrap App arguments in atoms by @jneem in https://github.com/tweag/nickel/pull/2287
* Print more information for git errors by @jneem in https://github.com/tweag/nickel/pull/2283
* [RFC007] Improve the runtime value representation (not yet used by the interpreter) by @yannham in https://github.com/tweag/nickel/pull/2282, https://github.com/tweag/nickel/pull/2288, https://github.com/tweag/nickel/pull/2290, https://github.com/tweag/nickel/pull/2297, https://github.com/tweag/nickel/pull/2303
* Add format --check by @bthmc in https://github.com/tweag/nickel/pull/2309
* [nix-experimental] Provide the proper base path to Nix with `%eval_nix%` by @yannham in https://github.com/tweag/nickel/pull/2314

Documentation
-------------

* Fix formatting of doc/manual/package-management.md by @yannham in https://github.com/tweag/nickel/pull/2274
* Fix missing package field in package manual example by @yannham in https://github.com/tweag/nickel/pull/2275
* Update examples to use latest Nickel idioms by @yannham in https://github.com/tweag/nickel/pull/2289

Version 1.12 (2025-06-04)
=========================

Nickel 1.12 includes the long-awaited addition of field punning, in the form of
include expressions, which makes it easy to re-use a variable as a record field
without running into infinite recursion errors or clumsy renamings. If `foo` is
a symbol in the environment (e.g. a variable defined earlier or a function
parameter), one can now write `{ include foo }` instead of having to write `let
foo_ = foo in { foo = foo_ }` to put it in a field with the same name. See the
syntax section of the manual for more details.

The Nickel CLI now accepts any supported configuration format as file input,
making the `nickel` binary a possible merge engine for JSON, YAML or TOML
configurations. for example, `nickel export conf1.json conf2.toml --format YAML
--output result.yaml` will merge a JSON configuration and a TOML configuration
into a YAML one using Nickel's merge semantics. Together with the just added
`--apply-contract` argument, it's also possible to use Nickel as a non-invasive
validator for existing configurations without needing to convert them to Nickel
first. For example, `nickel eval config.json --apply-contract schema.ncl` will
validate `config.json` against the Nickel contract `schema.ncl`.

Core language
-------------

* [Feat] Field punning (include expressions) by @yannham in https://github.com/tweag/nickel/pull/2241
* [Feat] Support annotations on included expressions by @yannham in https://github.com/tweag/nickel/pull/2252

LSP
---

* Add context to error opening trace file in nls by @L0r3m1p5um in https://github.com/tweag/nickel/pull/2244
* Update behavior of debugLog flag in VSCode extension by @L0r3m1p5um in https://github.com/tweag/nickel/pull/2257
* Filter out LSP diagnostics for labels with no message by @jneem in https://github.com/tweag/nickel/pull/2263

Tooling
-------

* Improved error message when subtyping an inhomogeneous record as a dict by @jneem in https://github.com/tweag/nickel/pull/2234
* Only output up to 10 warnings by @jneem in https://github.com/tweag/nickel/pull/2239
* Negation error messages by @jneem in https://github.com/tweag/nickel/pull/2242
* [Feat] Add `--apply-contract` argument to the CLI by @yannham in https://github.com/tweag/nickel/pull/2266
* [Feat] Determine the format automatically for input files on the CLI by @yannham in https://github.com/tweag/nickel/pull/2267

Fixes
-----

* Don't quote enum tag keywords by @jneem in https://github.com/tweag/nickel/pull/2232
* Remove the unused merge keyword by @jneem in https://github.com/tweag/nickel/pull/2243
* Bigger stack on windows by @jneem in https://github.com/tweag/nickel/pull/2260

Version 1.11 (2025-04-11)
=========================

Nickel 1.11 includes a new experimental package manager. It is integrated
directly in the normal Nickel binary, but is not enabled by default: you either
need to use a package-enabled pre-built binary (ending with `-pkg`) or build
Nickel from source with the feature `package-experimental`. Please refer to the
new package management chapter of the manual for more details.

Nickel 1.11 also includes a number of additions to the stdlib, improvements in
tooling (environment variable passing), the stdlib, the contract system, and
string interpolation (many cases don't need the explicit `std.to_string`
anymore). See the detailed changelog below for more details.

The large refactoring splitting up the internal representation into two
intermediate representations, started a few minor versions ago and paving the
ground for a more efficient interpreter, has been completed. Users should not
see much difference, although it already leads to better information in the LSP.

Breaking changes
----------------

* The typechecking of chains of imports has been modified due to the migration to
  a new internal intermediate representation. Before, when importing file A in
  a statically typed block, if the imported file was itself a single import of
  B, and B contained, say, the number `2`, then the typechecker would follow the
  chain and infer the type `Number` for the initial import of A. Now, the
  typechecker only looks at the apparent type of A but doesn't follow imports
  chains further. If the import expression in A doesn't have a type annotation,
  as in `import "B.ncl"`, its type will be inferred to be `Dyn` instead.

  If this breaks your code, the solution is to add missing type annotations to
  the intermediate imports, here `import "B.ncl" : Number`.

Core language
-------------

* Allow custom contracts to customize the label by @jneem in https://github.com/tweag/nickel/pull/2176
* Automatically convert interpolated values to string by @yannham in https://github.com/tweag/nickel/pull/2195

Documentation
-------------

* Describe customize mode in the CLI chapter of the manual by @yannham in https://github.com/tweag/nickel/pull/2219

Stdlib
------

* Adds a Matches contract to std.string by @jneem in https://github.com/tweag/nickel/pull/2172
* Adds a FieldsMatch contract for validating record fields against a regex by @jneem in https://github.com/tweag/nickel/pull/2174
* Add std.cast by @jneem in https://github.com/tweag/nickel/pull/2184

LSP
---

* Bump the VSCode extension to 0.4 by @yannham in https://github.com/tweag/nickel/pull/2162
* Support pull diagnostics and use them for testing by @jneem in https://github.com/tweag/nickel/pull/2166
* ADT and package management improvements in nls by @jneem in https://github.com/tweag/nickel/pull/2217

Tooling
-------

* Add support for environment variables sigil @env on the command line by @yannham in https://github.com/tweag/nickel/pull/2201
* Experimental package manager:
  * Package management CLI, part 1 by @jneem in https://github.com/tweag/nickel/pull/2146
  * Support for index packages by @jneem in https://github.com/tweag/nickel/pull/2175
  * Support for index packages with version resolution. by @jneem in https://github.com/tweag/nickel/pull/2183
  * Fix error message for bad package ids by @jneem in https://github.com/tweag/nickel/pull/2209

Fixes
-----

* Fix panic with multiple contracts on field by @yannham in https://github.com/tweag/nickel/pull/2199
* Traverse enum variants by @jneem in https://github.com/tweag/nickel/pull/2214

Version 1.10 (2025-02-11)
=========================

Nickel 1.10 includes various bug fixes and quality of life improvements. Nickel
now comes with more prebuilt binary packages (adding Windows and arm-based
MacOS), is now built with LTO (Link-Time Optimization), and comes with official
python bindings to be published on PyPI together with this release.

Under the hood, a lot of work has been devoted to internal refactorings in order
to prepare the implementation of a bytecode compiler and virtual machine
(RFC007). Those changes shouldn't have any noticeable effects currently for
users.

Breaking changes
----------------

* Record freezing ([#2113](https://github.com/tweag/nickel/pull/2113),
  [#2131](https://github.com/tweag/nickel/pull/2131)). To fix an unsound and
  unexpected behavior appearing when first altering a record with dictionary
  operations (`std.record.remove`, `std.record.insert` or `std.record.update` -
  see [#1877](https://github.com/tweag/nickel/issues/1877) and then overriding it,
  the aforementioned stdlib operations now **freeze** the returned record,
  which removes the possibility of performing further recursive overriding.

  Typically, the record returned by one of those operations is a static
  dictionary instead of a record with recursive dependencies. While you can
  still override specific fields through merging, the information about internal
  dependencies is lost and their reverse dependencies won't be updated
  automatically.

Documentation
-------------

* Mention any_of as alternative to enum by @yannham in [#2119](https://github.com/tweag/nickel/pull/2119)
* Link to the CLI chapter in the manual intro by @yannham in [#2144](https://github.com/tweag/nickel/pull/2144)

Stdlib
------

* Add the package std module by @jneem in [#2104](https://github.com/tweag/nickel/pull/2104)
* Fix empty capture groups in regexes by @jneem in [#2109](https://github.com/tweag/nickel/pull/2109)
* Add `filter_map`, `dedup` and some variants to the stdlib by @yannham in [#2120](https://github.com/tweag/nickel/pull/2120)

LSP
---

* Fix crash in NLS by @jneem in [#2093](https://github.com/tweag/nickel/pull/2093)
* Fix NLS crash when typechecking parse errors by @jneem in [#2154](https://github.com/tweag/nickel/pull/2154)

Tooling
-------

* Add support for packages to nickel-lang-core by @jneem in [#2094](https://github.com/tweag/nickel/pull/2094)
* Add support warnings by @jneem in [#2086](https://github.com/tweag/nickel/pull/2086)
* Fixed `Debug` impl of `EvalOrDeserError` printing entire source of files by @rben01 in [#2118](https://github.com/tweag/nickel/pull/2118)
* Do deep eval for doctests by @jneem in [#2110](https://github.com/tweag/nickel/pull/2110)
* pass thru feature `nix-experimental` by @KiaraGrouwstra in [#2132](https://github.com/tweag/nickel/pull/2132)
* Add github action for packaging and publishing python packages to PyPI (#1592) by @vlcek in [#2126](https://github.com/tweag/nickel/pull/2126)
* Include a release artifact for nls by @jneem in [#2139](https://github.com/tweag/nickel/pull/2139)
* Update rustyline to 15.0 by @neuschaefer in [#2142](https://github.com/tweag/nickel/pull/2142)
* Add LTO to static builds by @jneem in [#2147](https://github.com/tweag/nickel/pull/2147)
* Add import_paths parameter to Python bindings by @yannham in [#2157](https://github.com/tweag/nickel/pull/2157)

Version 1.9 (2024-11-12)
========================

Nickel 1.9 includes various bug fixes and quality of life improvements.

Noteworthy additions are:

- let-blocks: declaring several variables at once instead of chaining
  `let ... in ...`
- explicit import: the ability to specify explicitly the format of an imported
  file (it was based on the file extension implicitly and would default
  to Nickel), e.g. as `import "foo.txt" as 'Json` or `import "bar.ncl" as
  'Text`.
- the addition of a `nickel test` command that can extract snippets from the
  in-code documentation (the `| doc` metadata) together with their expected
  result and run them. The feature is detailed in a new CLI chapter of the user
  manual.

Two important future evolutions have been designed and discussed through RFCs:
package management and a performant bytecode virtual machine. Those features
aren't implemented yet, but the initial designs have been agreed upon.

Breaking changes
----------------

* Formatting: the formatting of let bindings has been fixed and improved. To
  avoid a large, irrelevant diff on your next commits, we advise formatting your
  whole codebase first after upgrading to 1.9 in a separate commit.

Core language
-------------

* Let blocks by @jneem ([#2010](https://github.com/tweag/nickel/pull/2010),
  [#2031](https://github.com/tweag/nickel/pull/2031),
  [#2051](https://github.com/tweag/nickel/pull/2051))
* Fix unsound record contract deduplication by @yannham in https://github.com/tweag/nickel/pull/2042
* Explicit import format: `import "sample.html" as 'Text` by @vi
  ([#2036](https://github.com/tweag/nickel/pull/2036),
  [#2070](https://github.com/tweag/nickel/pull/2070))
* Thunks for resolved imports (detect import infinite loops and avoid work duplication) by @jneem in https://github.com/tweag/nickel/pull/2052
* Use a persistent vector to represent arrays instead of an `Rc<[..]>` by @jneem in [#2057](https://github.com/tweag/nickel/pull/2057

Documentation
-------------

* Add a manual chapter for the cli. by @jneem in https://github.com/tweag/nickel/pull/2065
* [RFC006] Package management by @jneem in https://github.com/tweag/nickel/pull/1983
* [RFC007] Bytecode interpreter by @yannham in https://github.com/tweag/nickel/pull/2045

LSP
---

* Remove the option for an external formatter in nls by @jneem in https://github.com/tweag/nickel/pull/2064
* Fix completions in incomplete field name position. by @jneem in https://github.com/tweag/nickel/pull/2069
* Bound the length of nls completions by @jneem in https://github.com/tweag/nickel/pull/2073

Tooling
-------

* Add `--format` argument to `nickel query` command by @suimong in https://github.com/tweag/nickel/pull/2015
* Adds a `nickel test` subcommand for testing examples in docs. by @jneem in https://github.com/tweag/nickel/pull/2020
* Detect infinite recursions in `nickel doc` by @yannham in https://github.com/tweag/nickel/pull/2055
* Strict typechecking mode by @jneem in https://github.com/tweag/nickel/pull/2077
* Switch to toml-edit for spanned deserialization fixing TOML deserialization bug by @jneem in https://github.com/tweag/nickel/pull/2074
* Make serde-wasm-bindgen optional in core by @akavel in https://github.com/tweag/nickel/pull/2089
* Move 'clap' crate dependency behind feature flag in core by @akavel in https://github.com/tweag/nickel/pull/2090

Version 1.8 (2024-09-09)
========================

After a summer break, the team is happy to release Nickel 1.8!

This release ships with an important rework of the contract system, and in
particular the interface of custom contracts. The new system is more intuitive
for users implementing their own contracts, and makes it possible to use a
limited form of boolean logic on contracts that have been added to the stdlib
(`std.contract.one_of`, `std.contract.all_of`, `std.contract.not`,
`std.contract.check`).

We advise reading the updated manual section on writing custom contracts and to
migrate your custom contracts to take advantage of the simplification of the
interface and those new operators. A lot of custom contract can most likely be
implemented as validators (see `std.contract.from_validator`).

The contract system rework is backward compatible and existing custom contracts
will continue to work as before. Though in the future, Nickel will probably emit
a warning of deprecation when a custom contract is defined as a naked function
instead of using one of the new contract constructor.

A limited form of subtyping has been added in 1.8 as well, making it easier to
use records as dictionaries in statically typed code. A new paragraph has been
added to the typing chapter of the manual accordingly.

Breaking changes
----------------

* Formatting: to better accommodate the new contract system, the formatting of
    function applications and definitions have been changed. To avoid a large,
    irrelevant diff on your next commits, we advise formatting your whole
    codebase first after upgrading to 1.8 in a separate commit.

Core language
-------------

* Contract system rework:
  * Do not evaluate types away by @yannham in https://github.com/tweag/nickel/pull/1954
  * Add a contract node to the AST by @yannham in https://github.com/tweag/nickel/pull/1955
  * Add dedicated node and constructor for general custom contracts by @yannham in https://github.com/tweag/nickel/pull/1964
  * Introduce validators for building custom contracts by @yannham in https://github.com/tweag/nickel/pull/1970
  * Split contracts into an immediate part and a delayed part by @yannham in https://github.com/tweag/nickel/pull/1975
  * Re-introduce unified representation for custom contracts, migrate builtin contracts by @yannham in https://github.com/tweag/nickel/pull/1987
* Allow some function equality comparison by @yannham in https://github.com/tweag/nickel/pull/1978
* Dictionary subtyping:
  * Add a subsumption rule between record types and dictionary types by @Eckaos in https://github.com/tweag/nickel/pull/1977
  * Add array constructor to subsumption rule by @Eckaos in https://github.com/tweag/nickel/pull/1991
  * Add dictionary constructor to subsumption rule by @Eckaos in https://github.com/tweag/nickel/pull/1998
  * Add record constructor to subtyping by @Eckaos in https://github.com/tweag/nickel/pull/2007
* Add record/split_pair and record/disjoint_merge by @yannham in https://github.com/tweag/nickel/pull/1982
* Optimize `std.contract.Equal` using %record/split_pair% by @yannham in https://github.com/tweag/nickel/pull/1988
* [Optimization] Cache contract generation and pre-compile some match expression by @yannham in https://github.com/tweag/nickel/pull/2013
* Fix and improve contract generation optimization for static types by @yannham in https://github.com/tweag/nickel/pull/2017

Stdlib
------

* Comparisons for number, string, array, and record by @jeremyschlatter in https://github.com/tweag/nickel/pull/1985
* Add boolean contract combinators to the stdlib by @yannham in https://github.com/tweag/nickel/pull/1995
* Add short-circuiting fold_left by @jneem in https://github.com/tweag/nickel/pull/2000
* Add trigonometric functions, pi, e and log to the number module by @Hugal31 in https://github.com/tweag/nickel/pull/2006

Documentation
-------------

* Pass on the contract chapter of the manual by @yannham in https://github.com/tweag/nickel/pull/1999
* Adding documentation for subtyping by @Eckaos in https://github.com/tweag/nickel/pull/1990

LSP
---

* Combine metadata for completion items instead of choosing arbitrarily. by @jneem in https://github.com/tweag/nickel/pull/1940
* Don't blacklist files forever by @thufschmitt in https://github.com/tweag/nickel/pull/1973
* Make the LSP configurable by @thufschmitt in https://github.com/tweag/nickel/pull/1974
* More aggressive type/contract deduplication on hover by @jneem in https://github.com/tweag/nickel/pull/1984

Tooling
-------

* Add span information for TOML imports by @yannham in https://github.com/tweag/nickel/pull/1949
* Update to latest topiary-queries (formatting) by @yannham in https://github.com/tweag/nickel/pull/2032

Version 1.7 (2024-06-11)
========================

Nickel 1.7 ships with several extensions to pattern matching which was
introduced in Nickel 1.5 but had a number of limitations. See below for further details.

Additionally, the publication of [Topiary](https://topiary.tweag.io/) to the
crates registry makes it so that all versions of Nickel from 1.7 and onward,
however they are built, ship with `nickel format` and the language server
formatting command working out of the box without needing a separate
installation of Topiary (which was previously required when installing from
crates.io using `cargo install nickel-lang-cli` or `cargo install
nickel-lang-lsp`).

Breaking changes
----------------

* Although primitive operators are internal and aren't officially part of any
    stability guarantee, some libraries sometimes need to resort to using them,
    when there's no equivalent in the stdlib. Nickel 1.7 had a big primop
    refactoring, amounting mostly to renaming. If you're using primops, please
    look at the corresponding [pull
    request](https://github.com/tweag/nickel/pull/1937)
* The behavior of destructuring has been changed to match exactly the behavior
    of pattern matching. While there should be no difference on well-behaving
    programs, this change makes destructuring stricter and can lead to some
    programs that were running fine before 1.7 to now fail with `unmatched
    pattern`. The typical example is when destructuring a record with a field
    that is not present: `let {x,y, ..} = import "lib.ncl" in`. If `x` is absent
    from the `lib.ncl` but is never used anywhere, this used to work fine before
    1.7 (the error would only trigger upon usage of `x`) but will now fail eagerly.

    If you need to fix a large codebase with long import destructuring chains
    and you don't know which fields are the offending ones, one possible
    technique is to open the importing file in an editor and use the `goto
    definition` command of the NLS on each field of a faulty
    pattern. If it works, the field is present, but if it doesn't, the field
    might be faulty.

Core language
-------------

* Patterns now support constant values (`1`, `null`, `"a"`, etc.), array
    patterns (`[fst, snd, ..tail]`), pattern guards (`'Foo x if std.is_number
    x`), wildcard patterns (`_` placeholder) and or-patterns (`('Foo x) or ('Bar
    x) or ('Baz x)`): (see the syntax section on of the manual for more details)
    * Implement wildcard patterns by @yannham in https://github.com/tweag/nickel/pull/1904
    * Implement constant patterns by @yannham in https://github.com/tweag/nickel/pull/1897
    * Implement pattern guards by @yannham in https://github.com/tweag/nickel/pull/1910
    * Implement array patterns by @yannham in https://github.com/tweag/nickel/pull/1912
    * Implement or-patterns by @yannham in https://github.com/tweag/nickel/pull/1916
* Uniformize destruct and pattern matching logic by @yannham in https://github.com/tweag/nickel/pull/1907
* Opaque values by @jneem in https://github.com/tweag/nickel/pull/1913

Stdlib
------

* Add `record.get_or` to get value from record supplying default value by @olorin37 in https://github.com/tweag/nickel/pull/1920
* [Fix] Avoid `record.get_or` failing on field without definition by @yannham in https://github.com/tweag/nickel/pull/1946
* Add stdlib function `array.at_or` by @olorin37 in https://github.com/tweag/nickel/pull/1927
* Add `std.enum.(from/to)_tag_and_arg` and `std.enum.map` to dynamically decompose and recompose an enum by @yannham in https://github.com/tweag/nickel/pull/1939

Documentation
-------------

* Fix typo in BlameError documentation by @ErinvanderVeen in https://github.com/tweag/nickel/pull/1899
* Update README.md for nix profile install by @cloudyluna in https://github.com/tweag/nickel/pull/1918

LSP
---

* Tell NLS about variable bindings in match statements by @jneem in https://github.com/tweag/nickel/pull/1926
* Add --version support to NLS and fix feature unification issues by @yannham in https://github.com/tweag/nickel/pull/1936
* Fix NLS crash and better refresh diagnostics by @jneem in https://github.com/tweag/nickel/pull/1944

Tooling
-------

* Include field path in non serializable error by @yannham in https://github.com/tweag/nickel/pull/1905
* Allow single input to `nickel xxx` command to be JSON, YAML or TOML as well by @olorin37 in https://github.com/tweag/nickel/pull/1902
* Use Topiary's published crates over git to always enable formatting by @ErinvanderVeen in https://github.com/tweag/nickel/pull/1919

Version 1.6 (2024-04-25)
========================

Nickel 1.6 is a maintenance release including several bug fixes and
improvements, in particular around the features introduced in Nickel 1.5 (enum
variants and background evaluation in the LSP).

Core language
-------------

* Extend merge to enum variants by @yannham in https://github.com/tweag/nickel/pull/1862
* [Fix] Allow multiple underscore to start identifiers by @yannham in https://github.com/tweag/nickel/pull/1884

Stdlib
------

* Add `std.string.find_all` by @fuzzypixelz in https://github.com/tweag/nickel/pull/1870
* Add empty optional fields-aware record operator variants by @yannham in https://github.com/tweag/nickel/pull/1876

Documentation
-------------

* modular-configurations.md: to_lower -> lowercase by @Jasha10 in https://github.com/tweag/nickel/pull/1857
* manual/modular-configurations.md: add argument to std.string.join by @Jasha10 in https://github.com/tweag/nickel/pull/1859
* manual/syntax.md: minor typo by @Jasha10 in https://github.com/tweag/nickel/pull/1860

LSP
---

* Improve diagnostic location in nls by @jneem in https://github.com/tweag/nickel/pull/1856
* Propagate pending array contracts in permissive_eval by @jneem in https://github.com/tweag/nickel/pull/1854
* Don't leak memory in background eval by @jneem in https://github.com/tweag/nickel/pull/1869
* Add a recursion limit to background evaluation by @jneem in https://github.com/tweag/nickel/pull/1878
* Dedup diagnostics by @jneem in https://github.com/tweag/nickel/pull/1883
* Extend the symbol range to include the rhs by @jneem in https://github.com/tweag/nickel/pull/1887
* Leverage function contract information by @yannham in https://github.com/tweag/nickel/pull/1888
* Fix LSP not showing type signature in untyped code by @yannham in https://github.com/tweag/nickel/pull/1889

Tooling
-------

* Markdown documentation generation: do not insert line breaks by @yannham in https://github.com/tweag/nickel/pull/1879
* [Fix] Nickel doc: fix missing newline in markdown output by @yannham in https://github.com/tweag/nickel/pull/1880
* Fix infinite recursion in doc symbols. by @jneem in https://github.com/tweag/nickel/pull/1881

Fixes
-----

* [Fix] Polymorphic field typechecking by @yannham in https://github.com/tweag/nickel/pull/1872
* Force enum payloads by @jneem in https://github.com/tweag/nickel/pull/1890

Version 1.5 (2024-03-12)
========================

Nickel 1.5 is a major release (albeit not literally), with new core language
features and improved LSP. In particular, Nickel 1.5 introduces:

- Full blown pattern matching. Patterns were previously restricted to
    destructuring let-bindings. They can now be used within match expressions as
    well (which only supported simple enum tags before).
- Enum variants. Enum variants are a new language constructs which are
    enum tags with associated data (they are applied to an argument). They can
    be seen as form of algebraic data types (ADT). Patterns, enum types,
    typechecking and other parts of the language are extended accordingly to
    support them.
- Background evaluation in the LSP. The LSP now performs evaluation of the
    current document in the background to report evaluation errors directly in
    your editor, and in particular contract errors.
- A new section of the manual on writing modular configurations.


Core language
-------------

* Allow metadata keywords in field position by @yannham in https://github.com/tweag/nickel/pull/1768
* Support other bases than decimal for num literals by @yannham in https://github.com/tweag/nickel/pull/1798

* Enum variants:
  * Structural ADTs: first step by @yannham in https://github.com/tweag/nickel/pull/1770
  * Implement equality on ADTs by @yannham in https://github.com/tweag/nickel/pull/1787
  * Fix laziness closurization bug, add support for ADTs by @yannham in https://github.com/tweag/nickel/pull/1789
  * ADT destructuring by @yannham in https://github.com/tweag/nickel/pull/1812
  * Enum tag destructuring by @yannham in https://github.com/tweag/nickel/pull/1813
  * Implement ADT contracts by @yannham in https://github.com/tweag/nickel/pull/1821
  * Introduce application syntax for ADTs by @yannham in https://github.com/tweag/nickel/pull/1825
  * Relax enum row conflicts by @yannham in https://github.com/tweag/nickel/pull/1831
  * Properly force enum variants by @yannham in https://github.com/tweag/nickel/pull/1835
  * Fix enum contract stripping unwrapping variant argument by @yannham in https://github.com/tweag/nickel/pull/1833

* Pattern matching:
  * [Refactor] Pattern matching by @yannham in https://github.com/tweag/nickel/pull/1799
  * Record pattern compilation by @yannham in https://github.com/tweag/nickel/pull/1816
  * Enum pattern compilation by @yannham in https://github.com/tweag/nickel/pull/1817
  * [Refactor] Pattern positions by @yannham in https://github.com/tweag/nickel/pull/1819
  * Full pattern matching by @yannham in https://github.com/tweag/nickel/pull/1820
  * Support pattern contracts in match statement by @yannham in https://github.com/tweag/nickel/pull/1823
  * Fix the semantics of default values in patterns by @yannham in https://github.com/tweag/nickel/pull/1826
  * Specialized pattern compilation for enum tags by @yannham in https://github.com/tweag/nickel/pull/1846

Stdlib
------

* Add `array.zip_with` and `array.map_with_index` to the standard library by @vkleen in https://github.com/tweag/nickel/pull/1797
* fixed std.array.split_at behavior at right boundary. by @suimong in https://github.com/tweag/nickel/pull/1803
* Update stdlib for ADTs by @yannham in https://github.com/tweag/nickel/pull/1822

Tooling
-------

* LSP: Add "goto definition" support for the import term by @jneem in https://github.com/tweag/nickel/pull/1756
* LSP: Add support for NICKEL_IMPORT_PATH environment variable @jneem in https://github.com/tweag/nickel/pull/1795
* LSP: Improved reference-finding by @jneem in https://github.com/tweag/nickel/pull/1800
* LSP: rename action by @jneem in https://github.com/tweag/nickel/pull/1811
* LSP: evaluation in the background by @jneem in https://github.com/tweag/nickel/pull/1814
* LSP: Improve document symbols by @jneem in https://github.com/tweag/nickel/pull/1848
* Add more spacing to contract error messages by @yannham in https://github.com/tweag/nickel/pull/1767
* Proper error message for non exhaustive match by @yannham in https://github.com/tweag/nickel/pull/1772
* Add InvalidContractError by @yannham in https://github.com/tweag/nickel/pull/1824

Documentation
-------------

* Add patterns to the syntax section of the manual by @yannham in https://github.com/tweag/nickel/pull/1832
* Improve the description of identifier syntax in the manual by @vkleen in https://github.com/tweag/nickel/pull/1839
* Add subsection on enum types in the manual by @yannham in https://github.com/tweag/nickel/pull/1836
* Fix old CLI syntax in documentation by @cydparser in https://github.com/tweag/nickel/pull/1844
* Add manual section on modular configurations by @yannham in https://github.com/tweag/nickel/pull/1841
* Update/refresh examples using latest Nickel idioms by @yannham in https://github.com/tweag/nickel/pull/1849

Fixes
-----

* Check if stderr is a terminal for error messages by @vkleen in https://github.com/tweag/nickel/pull/1766
* correctly drop Array::IntoIter by @Radvendii in https://github.com/tweag/nickel/pull/1773
* LSP: vendor codespan, and fix character offset issues by @jneem in https://github.com/tweag/nickel/pull/1793
* [Fix & Refactor] Row conflict error by @yannham in https://github.com/tweag/nickel/pull/1808
* Fix unbound identifier when querying in REPL by @yannham in https://github.com/tweag/nickel/pull/1843
* Fix --field not applying pending contracts by @yannham in https://github.com/tweag/nickel/pull/1778

Version 1.4
===========

Nickel 1.4 is a maintenance release, at the exception of a breaking change (see below).

Breaking changes
----------------

* The curried dot operator added in Nickel 1.3 was implemented the wrong way:
    the arguments were flipped, meaning that `(.) foo bar` was `bar."%{foo}"`
    instead of the expected `foo."%{bar}"`. While the initial flipped
    implementation seems more useful for piping operations using the reverse
    application operator `|>`, it's inconsistent with all other curried
    operators, where `(<operator>)` is always defined as `fun x y => x
    <operator> y`. To ensure consistency, and because the initial behavior was
    an oversight and not a conscious design decision, we decided to change the
    definition of `(.)` to match the other curried operator by flipping its
    arguments.

    To fill the gap, Nickel 1.4 introduces `std.record.get` with the same
    definition as the `(.)` introduced in Nickel 1.3. To migrate from 1.3 to
    1.4, you can either flip the arguments of the curried dot as a function
    `(.)` whenever possible, or you can just replace it with the new
    `std.record.get`.

    (implemented by @yannham in https://github.com/tweag/nickel/pull/1752)

Tooling
-------

* Search for imports in NICKEL_IMPORT_PATH by @jneem in https://github.com/tweag/nickel/pull/1716
* Add a cli param --import-path to specify the search path by @jneem in https://github.com/tweag/nickel/pull/1721
* LSP: Fix hover on assignments to subrecords by @jneem in https://github.com/tweag/nickel/pull/1725
* Print something when nickel doc succeeds by @yannham in https://github.com/tweag/nickel/pull/1729
* Add --error-format flag to serialize err diagnostics by @yannham in https://github.com/tweag/nickel/pull/1740
* LSP: get record completion in arrays by @jneem in https://github.com/tweag/nickel/pull/1746

Core language
-------------

* Support importing txt files as strings by @Quantum64 in https://github.com/tweag/nickel/pull/1734

Fixes
-----

* `nickel format`: don't fail silently on invalid input anymore by @yannham in https://github.com/tweag/nickel/pull/1749
* Update Topiary dependencies to correctly handle `(.)` in `nickel format` by @yannham in https://github.com/tweag/nickel/pull/1753

Version 1.3
===========

Version 1.3 includes several new optimizations following reports of long
evaluation time for medium-sized codebase. The command-line interface (CLI) has
been reworked to be more user-friendly, at the cost of breaking changes: see
below. Finally, the LSP has seen continuous improvement as well.

Breaking changes
----------------

* @vkleen improved the CLI UX in numerous ways in https://github.com/tweag/nickel/pull/1632
  - The file argument is now argument positional. That is, instead of running
    `nickel export -f config.ncl`, now use `nickel export config.ncl` instead.
  - Every command which can take a file argument can now take several of them.
    The program parsed from the files are then merged before applying the
    action. For example, the new
    `nickel export config1.ncl config2.ncl config3.ncl` is the equivalent of the
    previous:
    `nickel export <<< '(import "config1.ncl") & (import "config2.ncl") & (import "config3.ncl")'`
  - Evaluation is now an explicit subcommand, instead of being the default
    action. Instead of running `nickel -f config.ncl` to evaluate a file, use
    `nickel eval config.ncl` instead.
* Not a breaking change per se, because the customize mode is experimental, but
  @yannham introduced a new syntax for customize mode in
  https://github.com/tweag/nickel/pull/1709. Instead of dynamically generating a
  CLI where arguments are field paths, the new customize mode CLI directly take
  assignments written in a Nickel-like syntax as positional arguments. For
  example, in 1.2, the command
  `nickel eval -f confing.ncl -- \
    --input.field1 '"Value"' --input.flag false \
    --override output.bar 0`
  now becomes
  `nickel eval config.ncl -- \
    'input.field1="Value"' input.flag=false \
    --override output.bar=0`

Fixes
-----

* Fix `record.update` by making `record.insert` act consistently by @yannham in https://github.com/tweag/nickel/pull/1669

Tooling
-------

* LSP:
  - implement type-based completion in the new completer by @jneem in https://github.com/tweag/nickel/pull/1577
  - Improve context completion by @jneem in https://github.com/tweag/nickel/pull/1584
  - Take the ancestor path into account when env-completing from uncles. by @jneem in https://github.com/tweag/nickel/pull/1661
  - Add goto support for pattern bindings by @jneem in https://github.com/tweag/nickel/pull/1665
  - Add cousin search to goto and hover by @jneem in https://github.com/tweag/nickel/pull/1670
  - Improve hover output for let patterns by @jneem in https://github.com/tweag/nickel/pull/1696
  - First prototype of contract evaluation by @jneem in https://github.com/tweag/nickel/pull/1672
* LSP: a large refactoring work by @jneem to get rid of the old and
  hard-to-maintain code analysis implementation
  (https://github.com/tweag/nickel/pull/1623,
    https://github.com/tweag/nickel/pull/1629,
    https://github.com/tweag/nickel/pull/1658,
    https://github.com/tweag/nickel/pull/1663)
* Honor `nostdlib` in customize mode as well by @vkleen in https://github.com/tweag/nickel/pull/1634
* Add the `list` subcommand to the customize mode by @yannham in https://github.com/tweag/nickel/pull/1709
* add %eval_nix% primop for Nix interop by @Radvendii in
  https://github.com/tweag/nickel/pull/1465 (requires to build with the
  corresponding experimental feature enabled)
* Get rid of shallow_repr and print full terms in error messages by @yannham in
  https://github.com/tweag/nickel/pull/1676
* Add suggestions to the error message when misspelling a record field by @yannham in https://github.com/tweag/nickel/pull/1710
* Add a `--field` argument to subcommands to target a specific field whenever it makes sense by @yannham in https://github.com/tweag/nickel/pull/1712

Optimizations
-------------

* Contract elision for static types by @yannham in https://github.com/tweag/nickel/pull/1671
* Implement contract deduplication optimization by @yannham in https://github.com/tweag/nickel/pull/1631
* Array contract deduplication by @yannham in https://github.com/tweag/nickel/pull/1674
* Get rid of most generated variables by @yannham in https://github.com/tweag/nickel/pull/1679

Documentation
-------------

* Fix invalid example code in doc of blame_with_message by @bgni in https://github.com/tweag/nickel/pull/1689
* Fix doc, example code for pipe lacks prefix by @bgni in https://github.com/tweag/nickel/pull/1692
* change nickel-nix to organist by @Radvendii in https://github.com/tweag/nickel/pull/1691

## New Contributors

* @bgni made their first contribution in https://github.com/tweag/nickel/pull/1689
* @giorgiga made their first contribution in https://github.com/tweag/nickel/pull/1697

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
* Normalize line endings in string literals during parsing (Windows compatibility) by @vkleen in https://github.com/tweag/nickel/pull/1562
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
* LSP: various improvements to completion by @jneem (https://github.com/tweag/nickel/pull/1450, https://github.com/tweag/nickel/pull/1473)
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
- Parts of the syntax and some builtin symbols (types, stdlib functions, and so
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
