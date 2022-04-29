# Why Nickel ?

There already exist quite a few languages with a similar purpose to Nickel:
[CUE](https://cuelang.org/), [Dhall](https://dhall-lang.org/),
[Jsonnet](https://jsonnet.org/),
[Starlark](https://docs.bazel.build/versions/master/skylark/language.html), to
mention the closest contenders. So why Nickel ?

Nickel originated as an effort to detach the [Nix](https://nixos.org/)
expression language from the Nix package manager, while adding typing
capabilities and improve modularity. We found that in practice, Nix is a simple
yet expressive language which is particularly well fitted to build programmable
configurations, and that although other good solutions existed, no one was
entirely satisfying for our use-cases (mainly Nix, cloud infrastructure and
build systems). Let's review the design choices of Nickel, why they were made,
and how they compare with the choices of the four aforementioned
alternatives.

## Table of contents

1. [Design rationale](#design-rationale)
    - [Functions](#functions)
    - [Typing](#typing)
    - [Turing completeness](#turing-completeness)
    - [Side-effects](#side-effects)
2. [Comparison with alternatives](#comparison-with-alternatives)
    - [Starlark](#starlark-the-standard-package)
    - [Nix](#nix-json-and-functions)
    - [Dhall](#dhall-powerful-type-system)
    - [CUE](#cue-opinionated-data-validation)
    - [Jsonnet](#jsonnet-json-functions-and-inheritance)

## Design rationale

### Functions

The main contribution of a configuration language over a static configuration is
*abstraction*: make the same code reusable in different contexts by just varying
some inputs, instead of pasting variations of the same chunks all over the
codebase, making them hard to maintain and to extend.  Abstraction is achievable
by several means: for example, a pure object oriented language like Java uses
objects as a primary structuring block.

Nickel (and other languages of the list, for that matter) uses *functions* as a
basic computational block. Functions are simple and well understood (some inputs
give an output), pervasive (as macros, procedures, methods, etc.), and
composable.  Nickel is *functional*, in the sense that functions are moreover
first-class: they can be created everywhere, passed around as any other value,
and called at will.

### Typing

One recurring difference between Nickel and other configuration languages is
that Nickel has a static type system. The trade-offs of static typing for
configurations are different than in the case of a general purpose programming
language.

#### Reusable versus specific code

We can divide code in two categories:

1. Configuration-specific code: local code that will only be used for the
   generation of said configuration.
2. Reusable code: code that is used in several configurations and will be
    potentially used in many more. Basically, library code.

As opposed to a traditional program which interacts with external agents (a
user, a database, a web service, ...), configuration-specific code will
always be evaluated on the same inputs. Thus any type error will be visible at
evaluation time anyway. In this case types can only get in the way, as they may
require annotations and forbids correct but non typable code, while not really
adding value.

On the other hand, reusable code may be called on infinitely many different inputs:

```nickel
let f x = fun x => if x < 1000 then x + 1 else x ++ 2
```

In this contrived but illustrative example, `f` can work fine on a thousand
inputs, but fails on the next one. Functions in general can never be tested
exhaustively.  Meanwhile, static typing would catch the typo `x ++ 2` even
before the first usage.

To this problem, Nickel offers the solution of a gradual type system which
supports a mix of both typed and non typed parts, with the following
perks:

- You get to chose when to use static typing or not.
- You can write code without any type annotation even when calling to statically
  typed code.
- You can start with a totally untyped codebase and gradually (hence the
    name) type it parts by parts.
- Nickel automatically insert checks at the boundary between the typed and the
    untyped world to report type mismatches early.

#### Typing JSON

The second motivation for a non fully static type system is that some code may
be hard to type. JSON is a de-facto standard format for configuration and Nickel
aims at being straightforwardly convertible to and from JSON. If it were to be
fully statically typed, it would have to type things like heterogeneous lists:
`[{ field: 1 }, { differentField: 2}]`, which is doable but not trivial (see the
[comparison with Dhall](#Dhall:-powerful-type-system)). Nickel made the choice
of offering typing capabilities for common idioms, but when the type system
falls short of expressivity, you can still write your code without types.

#### Data validation

Another peculiarity is that there is an external tool which will consume the
configuration at the end. The generated configuration has to conform to a
specification dictated by this tool, which is a priori alien to the generating
program.

In the following example,

```json
{
  ...
  "id": "www.github.com/nickel/back",
  "baseURL": 2
}
```

the configuration language has no reason to suspect that `id` and `baseURL`
contents have been mistakingly swapped. It would need to be aware of the fact
that `id` should be an integer and `baseURL` a string. Surely, an error will
eventually pop up downstream in the pipeline, but how and when? Will the bug be
easy to track down if the data has gone through several transformations, inside
the program itself or later in the pipeline ?  Using types, the generating
language is no more oblivious to these external schemas and can model them
internally, enabling early and precise error reporting.

In Nickel, such schemas are specified using *enriched values*. Enriched values
are meta-data about record fields like `id` or `baseURL`: they can provide
documentation, a default value, or even a type. Types so specified are called
*contracts*: they are not part of the static type system, but rather offer a
principled approach to dynamic type checking. They enforce types (or more
complex, user-defined) assertions at runtime. Equipped with enriched values, one
can for example ensure that `baseURL` is not only a string but a valid URL, and
document that it should be the Github homepage of a project.

### Turing completeness

All listed languages but Jsonnet forbid general recursion, and are hence non
Turing-complete. The idea is that generating configuration should always
terminate, and combinators on collections (e.g. `map` or `fold`) - or equivalent
bounded loops - are enough in practice: why take the risk of writing programs
stuck in an infinite loop for no reward ? On the other hand, one can write
programs with huge running time and complexity even in a language which is not
Turing-complete \[1\].  Also, while configuration-specific code almost
never requires recursion, this is not the case with library code. Allowing
recursion makes it possible for programmers to implement new generic
functionalities \[2\].

\[1\]: [Why Dhall is not Turing complete](http://neilmitchell.blogspot.com/2020/11/turing-incomplete-languages.html)\
\[2\]: [Turing incomplete languages](http://www.haskellforall.com/2020/01/why-dhall-advertises-absence-of-turing.html)

### Side-Effects

As for Turing-completeness, most of these languages also forbid side-effects.
Side-effects suffer from general drawbacks: they make code harder to reason
about, to compose, to refactor and to parallelize. In general-purpose
programming languages they are a necessary evil, the game being to circumscribe
their usage and limit their effects. However, they may not be necessary at all
for a configuration language, which has no reason to mess with the file system
or to send a network packet. External, fixed inputs may be provided as inputs to
the program without requiring it to interact directly with, say, environment
variables.

However, sometimes the situation does not fit in a rigid framework: as for
Turing-completeness, there may be cases which mandates side-effects. An example
is when writing [Terraform](https://www.terraform.io/) configurations, some
external values (an IP) used somewhere in the configuration may only be known
once another part of the configuration has been evaluated and executed
(deploying machines, in this context). Reading this IP is a side-effect, even if
not called so in Terraform's terminology.

Nickel permits side-effects, but they are heavily constrained: they must be
commutative, a property which makes them not hurting parallelizability. They are
extensible, meaning that third-party may define new effects and implement
externally the associated effect handlers in order to customize Nickel for
specific use-cases.

## Comparison with alternatives

Let's compare Nickel with the languages cited at the beginning: Starlark, Nix
expressions, Dhall, CUE, Jsonnet.

### Starlark: the standard package

Starlark is a language originally designed for the [Bazel](https://bazel.build/)
build system, but it can also be used independently as a configuration language.
It is a dialect of Python and includes the following classical features:

- First-class functions: abstraction and code-reuse
- Basic data structure: list and dictionaries
- Dynamic typing: no type annotations

With the following restrictions:

- No recursion: the language is not Turing-complete
- No side-effects: execution cannot access the file system, network or system clock.

In summary, Starlark comes with a sensible basic set of capabilities which is
good enough to enable the writing of parametrizable and reusable configurations.

### Starlark vs Nickel

Starlark forbids recursion and side-effects which are allowed in Nickel. It
lacks a static type system, which hampers the ability to write robust library
code and prevents the expression of data schemas inside the language.

### Nix: JSON and functions

Nix (sometimes call Nix expressions in full) is the language used by the [Nix
package manager](https://nixos.org/). It is a direct inspiration for Nickel, and
writing packages for Nix is an important target use-case.

Nix has a simple core: JSON datatypes combined with higher-order functions,
recursion and lazy evaluation. The Nix language is rather tightly integrated
with the Nix package manager, making it not trivial to use as a standalone
configuration language. Its builtins, including a few side-effects, are also
oriented toward the package management use-case.

### Nix vs Nickel

Nickel builds on the same core as Nix (JSON plus functions), and is in fact not
far from being a superset of the Nix language.

However, Nix lacks any native typing and validation capabilities, which Nickel
brings through static typing and contracts.

The merge system of Nickel is also in part inspired from the NixOS module
system. The NixOS module system has similar concepts but is implemented fully as
a Nix library. The rationale behind the merge system of Nickel is to bring back
merging into the scope of the language itself, bringing uniformity and
consistency, and potentially improving performance and error messages.
Additionally, native merging is also more ergonomic: in Nickel, merging doesn't
rely on an external module system, but works out of the box with plain records,
making it to use for other targets than Nix. Data validation directly leverages
metavalues and the contract system, instead of user-defined patterns such as
`mkOption` and the like (making them in particular discoverable by e.g. code
editors and IDEs)

### Dhall: powerful type system

Dhall is heavily inspired by Nix, to which it adds a [powerful type
system](https://github.com/dhall-lang/dhall-lang/blob/master/standard/README.md#summary).
Because of its complexity, the type system only supports a limited type
inference.  This can lead to code that is sometimes heavy on type annotations,
as in the following example:

```dhall
let filterOptional
    : ∀(a : Type) → ∀(b : Type) → (a → Optional b) → List a → List b
    =   λ(a : Type)
      → λ(b : Type)
      → λ(f : a → Optional b)
      → λ(l : List a)
      → List/build
        b
        (   λ(list : Type)
          → λ(cons : b → list → list)
          → λ(nil : list)
          → List/fold
            a
            l
            list
            (   λ(x : a)
              → λ(xs : list)
              → Optional/fold b (f x) list (λ(opt : b) → cons opt xs) xs
            )
            nil
        )

in  filterOptional
```

As stated in the [reusable vs specific](#reusable-versus-specific-code) section,
configuration-specific code does not benefit much from static typing. Functions
used as temporary values in such code, for example the anonymous function in
`map (fun x => x ++ ".jpg") baseFilesList`, require type annotations in Dhall.

Another point is that code is sometimes difficult to type, as raised in [typing
JSON](#typing-json). Typically, Dhall lists must be homogeneous: all elements
must have the same type. In particular, you can't represent directly the
following list of objects with different structure, which is valid JSON `[{a:
1}, {b: 2}]`. One has to write:

```dhall
let Union = < Left : {a : Natural} | Right : {b : Natural} >
in [Union.Left {a = 1}, Union.Right {b = 2}]
```

and write boilerplate code accordingly when manipulating this list.

### Dhall vs Nickel

Dhall is entirely statically typed, with an expressive but complex type system.
It requires type annotations, and may add boilerplate for code that is hard to
type, while Nickel prefers the mixed approach of gradual typing. As Starlark,
and as opposed to Nickel, Dhall forbids recursion and side-effects.

### CUE: opinionated data validation

CUE is quite a different beast. It focuses on data validation rather than
boilerplate removal. To do so, it sacrifices flexibility by not supporting not
only general recursion, but even general functions, in exchange of a
particularly well-behaved system. In CUE, everything is basically a type:
concrete values are just types so constrained that they only have one
inhabitant. These types form a lattice, which means they come with a union and
an intersection operation.

This provides:

- Merging: combine mixed schemas and values together in a well behaved way
  (merge is commutative, everywhere defined and idempotent)
- Querying: synthesize values inhabiting a type
- Trimming: Automatically simplify code

Nickel's merge system and enriched values are inspired by CUE's type lattice,
although the flexibility of Nickel necessarily makes the two system behave
differently.

### CUE vs Nickel

CUE is an outsider. While it produces elegant code, is backed by a solid theory
and is excellent at data validation, it seems less adapted to generating
configuration in general. It is also heavily constrained, which might be
limiting for specific use-cases.

### Jsonnet: JSON, functions and inheritance

In this list, Jsonnet is arguably the closest language to Nickel. As Nickel, it
is a JSON with higher-order functions, recursion and lazy evaluation. It
features a simplified object system with inheritance, which achieves similar
functionalities to Nickel's merge system.

### Jsonnet vs Nickel

The main difference between Jsonnet and Nickel are types. Jsonnet does not
feature static types, contracts or enriched values, and thus can't type library
code and has no principled approach to data validation.

### Summary

| Language | Typing                        | Recursion  | Evaluation | Side-effects                                      |
|----------|-------------------------------|------------|------------|---------------------------------------------------|
| Nickel   | Gradual (dynamic + static)    | Yes        | Lazy       | Yes (constrained)                                 |
| Starlark | Dynamic                       | No         | Strict     | No                                                |
| Nix      | Dynamic                       | Yes        | Lazy       | Predefined and specialized to package management  |
| Dhall    | Static (requires annotations) | Restricted | Lazy       | No                                                |
| CUE      | Static (everything is a type) | No         | Lazy       | No, but allowed in the separated scripting layer  |
| Jsonnet  | Dynamic                       | Yes        | Lazy       | No                                                |

## Conclusion

We outlined our motivations for creating Nickel, our main design choices and why
we made them. To give an idea of the position of Nickel in the ecosystem, we
compared it to a handful of related languages.  They are all very well designed
and offer working solutions for configuration generation, but we felt like there
was still room for a simple but expressive functional language, with a type
system hitting a sweet spot between expressiveness and ease-of-use, a nice way
of expressing data schemas inside the language and a merge system for easy
modularity.
