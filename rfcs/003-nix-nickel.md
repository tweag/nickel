---
feature: nix-nickel
start-date: 2022-04-20
author: Yann Hamdaoui
---

# Nix-Nickel

The goal of the present RFC is to lay out a compelling plan to integrate Nix and
Nickel. We want to use Nickel as a front-end language for writing Nix
expressions in a seamless way -- or, at least, such that there is no aspect in
which Nickel is unreasonably worse than Nix expressions (and will be hopefully
better in others).

This document is structured as follows:

1. [Motivation](#motivation): exposes the motivation and the scope of this RFC,
   and gives concrete examples of what we want to achieve.
2. [Challenges](#challenges): identifies the technical challenges, as of today,
   raised by the goals defined in 1.
3. [Proposal](#proposal): provides concrete proposals to overcome those
   challenges and achieve the goals of 1.

## Motivation

Nickel is a general-purpose configuration language. Yet, Nix is one very special
use-case. Nix has been the main motivation for creating Nickel in the first
place. Now that Nickel has been released, the next step is to offer a compelling
experience for using Nickel in place of Nix as a language to write packages,
flakes and modules.

### Constraints

Because Nix is a distinct project from Nickel which has existed for quite some
time now, we need operate under the following constraints:

- _Do not require unreasonable changes to Nix itself_. While it's probably
    illusory to avoid any change in Nix at all, we must strive to keep them
    reasonably small and undisturbing for regular Nix users, at least in the
    medium term.
- _Implementable in a reasonable timeline, if possible incrementally_. This RFC
    aims at proposing a solid vision for the future and not a temporary
    work-around. Still, given the finite bandwidth of the Nickel team, the
    proposed solution should be implementable in limited time (in the order of
    magnitude months, up to half a year). If possible, the plan should have
    intermediate milestones enabling new capabilities in an incremental way.
- _Backward-compatible_. We will realistically need to be able to leverage
    existing Nix code (especially from Nixpkgs) in some way: otherwise, even in
    the unlikely best case scenario where everybody suddenly switches to Nickel
    right away, it'll still take up a very long time to migrate everything.
- _Do not lock ourselves in the current Nixpkgs architecture_. Nixpkgs had
    made a number of design choices that are, in hindsight, not optimal (stdenv,
    describing packages as functions instead of data, etc.). While we have to
    keep some form of backward-compatibility, we want to do so while avoiding to
    tie ourselves to the current design and implementation. In other words, this
    document must propose a solution that stay compatible with a future radical
    departure from e.g. the Nixpkgs architecture.

### Scope

Nix expressions are used in related but different ways, each coming with varying
goals and constraints:

- **Nixpkgs**: derivations and packages in the original style.
- **Flakes**: new format for decentralized and composable packages.
- **NixOS modules**: NixOS system configuration.

In the long term, we aims at handling all those cases, but the scope of such an
undertaking appears very large for a single RFC. We decide to focus on the first
item: writing derivations and Nixpkgs-style packages.

The rationale is that derivations are the foundational concept on which the
whole Nix model is based. Flakes and NixOS modules build on derivations, as
illustrated by the following excerpt from a NixOS module:

```nix
systemd.myUnit.UnitConfig.ExecStart = pkgs.writeScript "foo" "..."
```

Something as innocent as using `pkgs.writeScript` in an hypothetical NixOS
Nickel module already implies to know how to generate derivations.

Thus, tackling derivations first makes sense. From there, Nixpkgs is probably
the thinnest layer over derivations of the three, and already opens a lot of
possibilities, so we choose to include it as well.

## Challenges

### Interaction with Nixpkgs

There is a bunch of scenarios that require leveraging Nixpkgs:

- **PKG**: Using a package from Nixpkgs, for example as a dependency
- **LIB**: Use one of the myriad of helpers from Nixpkgs: `mkShell`,
    `mkDerivation`, `writeText`, etc.
- **OVD**: Use a package from Nixpkgs but overriding some of its parameters

**PKG** could be done in a simple way, by just passing around derivations from
Nix to Nickel. Derivations are fully evaluated data that can be encoded as e.g.
JSON.

**LIB** is more involved. Some of Nixpkgs helpers take either functions, but the
sole laziness of expressions means that an interface (in the form of a FFI)
between Nix and Nickel would needs to handle going back and forth between the
two languages transparently.

**OVD** is technically not very different than **LIB**, since it mostly amounts
to calling Nix functions like `override`, `overrideAttrs`, etc (for any override
that doesn't operate directly on a derivation, which most are). That is, solving
**LIB** would solve **OVD** as well.

### String contexts

[String
contexts](https://shealevy.com/blog/2018/08/05/understanding-nixs-string-context/)
is a very useful feature of Nix. In a Nix program, each string carries a context
recording which other derivations it depends on at runtime, by tracking
variables usage inside string interpolation. Nix can then automatically
determine the dependencies of a derivation.

Abandoning the automatic dependency management offered by string contexts in
Nickel is an unacceptable degradation of the quality of life of Nix users.

## Proposal

### What could Nix-Nickel look like

Let's imagine what would in practice our ideal way of writing packages in
Nickel. The main inspiration for this section is Eelco Dolstra's [reflection on
the Nix language of the future][nix-lang].

#### Package as data

Despite Nix being branded as a _functional_ package manager, a perhaps
surprising conclusion of Eelco Dolstra's document is that writing packages as
actual functions is retrospectively of questionable value. Functions need to be
applied (and thus arguments be produced) before we can access any data (which
hurts discoverability), their inputs are hard to override, etc. Overall,
functions are opaque _computations_, which makes them hard to inspect and to
patch. The reflection above pushes to switch to a model where packages are
rather _data_. Of course, computations still take place -- this is after all the
whole point of having a configuration language -- but the right representation
for a package may be better separated, exposing pure data and representing
computations as data dependencies.

Concretely, a derivation would be represented simply as a recursive record:

```nickel
builders.derivation = {

  # Interface

  name
    | doc "Name of the derivation, used in the Nix store path."
    | Str,

  version
    | doc "Version of the derivation, used in the Nix store path."
    | Str,
    | default = "",

  builder
    | doc "Command to be executed to build the derivation."
    | Path,

  args
    | doc "Arguments passed to the builder."
    | Array Str
    | default = [],

  outputs
    | doc "Symbolic names of the outputs of this derivation."
    | Array Str
    | default = ["out"],

  env
    | doc "Structured values passed to the builder."
    | {_: Str}
    # inherit doesn't exist in Nickel, but let's pretend, for conciseness
    = {inherit outputs},

  # Implementation

  drv
    | doc "The resulting store derivation."
    =
    (env & {
      name = "%{name}-%{version}";
      inherit builder args;
    })
    |> builtin.derivation
}

# ... other definitions building on this one

# actual package
pkgs.hello = builders.unix_package & {
  name = "hello",
  version = "1.12",
  description = "A program that produces a familiar, friendly greeting",
  license = licenses.gpl,

  enable_gui
    | doc "Enable GTK+ support."
    | Bool
    | default false,

  src = builders.fetchurl & { url = ..., sha256 = ... },

  buildInputs = if enable_gui then [ gtk ] else [],
}
```

Thanks to laziness, `nix` could extract fields like `name` or `version` directly
without having to provide inputs or to evaluate `drv`.

We call this approach the **PARM** (**P**ackage **A**s **R**records
**M**odel) thereafter.

#### Specifying dependencies

The `pkgs.hello` example unveils a difficulty about specifying dependencies. It
would work in a model where all the packages are defined in one place and
populate the same huge record that will give Nickelpkgs. But that would be
totally impracticable and anti-modular.

Writing the `pkgs.hello` in its own file in Nickel today would result in the
`gtk` occurrence raising an `unbound variable` error, and for a good reason:
`gtk` is used but seemingly defined nowhere. In practice, `gtk` is expected to
appear in the final recursive record that would Nickelpkgs. But from a language
point of view, allowing references to yet-to-be-defined identifiers, that is
having dynamic scoping, comes with its share of drawbacks: non-locality making
code harder to understand, inability to detect unbound variables statically
(incurring late error reporting), inability to properly typecheck such code,
etc.

Let us review possible suctions:

##### Explicit list

One possibility is to specify explicitly all the
dependencies as record fields without definition:

```nickel
# file hello.ncl
{
  # dependencies
  gtk | NickelPackage,

  name = "hello",
  version = "1.12",
  description = "A program that produces a familiar, friendly greeting",
  license = licenses.gpl,

  enable_gui
    | doc "Enable GTK+ support."
    | Bool
    | default false,

  src = builders.fetchurl & { url = ..., sha256 = ... },

  buildInputs = if enable_gui then [ gtk ] else [],

}
```

The contract may be optional as the infrastructure of an hypothetical Nickelpkg
would already apply the contract Package.

<!-- In this design, it would be nice to avoid having to repeat those
     dependencies in several places (like in inputs). -->

##### Dynamic scoping

<!-- DRAFT. not sure this is a good idea -->

A second solution would be to implement a form of dynamic scoping, but with a
special syntax, such that it doesn't impact normal uses of variables. Could
either have a special `self` or `super` keyword that refers to the final version
of the fixpoint, or a special syntax such as identifiers starting a specific
character:

```nickel
{
  name = "hello",
  version = "1.12",
  description = "A program that produces a familiar, friendly greeting",
  license = licenses.gpl,

  enable_gui
    | doc "Enable GTK+ support."
    | Bool
    | default false,

  src = builders.fetchurl & { url = ..., sha256 = ... },

  buildInputs = if enable_gui then [ super.gtk ] else [],

}
```

**TODO**: other solutions? The pkg subfield, but seems like a lesser version of
the first proposal. Bazel-like includes? From experience, the semantics is
confusing, it's tied to the filesystem structure. And in the end it's still a
strange form of dynamic scoping: if we are to do it, let go for a proper version.

#### Aren't we re-inventing flakes?

<!-- This is a draft answer, but the question remains. The information in this
     paragraph needs to be confirmed and clarified by Nix team members -->

[Flakes][nix-flakes] is a new feature of Nix that make it easy to write and
distribute composable packages, all in a more reproducible way. The recent
versions of Nix (>= 2.5) -- and in particular the CLI -- are centered around the
flakes model.

The schema of flakes looks closer to the PARM than to the Nixpkgs style. A flake
is a record (an attribute set in Nix terminology) with directly accessible
metadata (like `name`, `description`, etc.), an `inputs` field describing what
inputs are needed, and an `outputs` field which is a function producing
derivations.

However, this only concerns a few top-level attributes. Other fields of the
derivation, such as `version`, `license`, etc. are still hidden under the output
function. Flakes are not a alternative to Nixpkgs, but a schema working on top
of it. Building derivations in a flake still relies on Nixpkgs mechanisms like
`mkDerivation`, and the overriding mechanisms are the same as well. Flakes
solves related but distinct issues (package composition and reproducibility).

At first sight, it shouldn't be hard to mechanically wrap a package in the PARM
as a flake (in general, going from a "data" package to more "functional" package
is easy). On consideration, though, is that the schema of flakes could be reused
in the PARM, just for the sake of consistency.

**TODO**: clarify what this last sentence exactly means.

### Nickel and Nixpkgs

As underlined in [Interaction with Nixpkgs](#interaction-with-nixpkgs), an FFI
mechanism would require a bidirectional communication between Nix and Nickel.
This is made complicated by the nature of both languages, which are lazy and
functional, meaning that a Nix function called from Nickel may incur arbitrary
calls back to Nickel (and vice-versa) to apply functions or to evaluate thunks.
Not only does this seem complex, but also hard to be made efficient.

Another possibility is to have everything evaluate in only one world, either Nix
or Nickel. Practically, we could compile the Nix code to Nickel or vice-versa,
and run everything on one side only.

Compiling Nickel to Nix and run everything on the Nix side would have the
advantage of leveraging all the Nix primitives and machinery freely. However,
this solution simply nullifies a lot of the value proposition of Nickel. The
added value of contracts is mainly due to its careful design as a primitive
feature of the language, and while it could be simulated to some extent in Nix,
the error message and performances just couldn't be in par. Similarly, the merge
system being native is in part motivated by the perspective of better
performance and error message. This simply becomes moot if we compile those
features away back to plain Nix code.

On the other hand, compiling Nix to Nickel looks more appealing. Nickel is close
to being a superset of Nix (the only non-trivial missing features being the
`with` construct and string contexts, but the latter doesn't even really change
the semantics). This would preserve the Nickel advances, and we could leverage
directly any non-builtin Nix function, including all the Nixpkgs library
functions.

The potential drawbacks of this approach:

- Performance: Nickel is not expected to be in par with Nix at this point, so
  the performance of Nixpkgs compiled to Nickel might be prohibitively bad at
  first. On a positive note, this would provide a great benchmark suite and
  motivation to guide future performance improvements.
- Some builtin may incur effects, which are not trivial to combine with the
  execution model (revertible thunks). That being said, those effects are in
  practice idempotent and commutative (creating paths in the store, creating
  derivation, etc.), which should be fine.

Compilation could be done on the fly, as the compilation process would be rather
straightforward, or have a Nixpkgs snapshot pre-compiled to Nickel, or a mix of
both.

This solution requires to reimplement Nix builtins in Nickel (the nickel-nix
compatibility layer) . This is an interesting milestone in itself, because even
without a Nix-to-Nickel compiler, the compatibility layer would already make
writing derivations in pure Nickel possible.

- TODO: what about dynamic imports?
- TODO: what about using a Nickel pkgs from Nix?

#### Using Nixpkgs in the PARM

Using a package from Nixpkgs in the PARM with a Nix-to-Nickel compiler would be
straightforward, as we could evaluate anything to a derivation whenever needed.
One would need to use Nixpkgs functions and idioms to do e.g. overrides though,
but this seems pretty hard to avoid, as a systematic translation from the
Nixpkgs model to the PARM doesn't seem trivial, if even doable.

<!-- TODO: add examples of such interactions -->

### Supporting string contexts

#### Nix-style

Ideally, we would like to have a behavior similar to Nix string contexts to
automatically track dependencies in a transparent way (you don't have to think
about it in Nix, using strings in a natural way).

There is a tension between endowing Nickel with specific features for the Nix
use-case and keeping it a general and versatile language for configuration.

One solution is to support string contexts in Nickel exactly as in Nix. But that
would do feel _ad hoc_. Other use-cases would benefit from a similar but more
flexible mechanism for automatic dependency tracking. For example, in Terraform,
interpolated expressions can refer to values that are only known after certain
resources have been deployed. The Terraform evaluator thus uses a similar
mechanism to elaborate a deployment plan from string metadata. Using Nickel for
Terraform would need to replicate this mechanism in some way.

We propose to adopt a more generic mechanism for strings with context, which
behavior can be parametrized. Strings become (conceptually) pairs `(s, ctxt)`
where `ctxt` is an arbitrary data structure with additional structure:

```Rust
combine: Ctxt -> Ctxt -> Ctxt,
pure: Str -> Ctxt,
pure_exp: Expr -> Ctxt,
```

The above functions could either be provided by user code or directly as an
interpreter plug-in.

- Existing standard strings would be equivalent to having a trivial context
  `null` and the obvious corresponding trivial structure.
- Custom contexts would be introduced by specific string delimiters, such as
    `nix%" %{pkgs.hello}/bin/hello"%`
- Different kind of strings are incompatible, to avoid accidentally forgetting
  or losing contexts. Also, we don't know a priori how to convert one context to
  another.
- There would be a contract to distinguish the different kind of
  strings in order enforce the usage of e.g. Nix style contexts for writing
  Nickel for Nix. We want to avoid users loosing or missing context unknowingly.

#### G-exps

Alternative: something like `g-exp`. No magic, no extension, but less ergonomic.

#### Effects

Another possible route is to use [effects][nickel-effects].

<!-- TODO: add a proposal using effects. If string interpolation can perform
effects, including actual deployment (and not just build free effects AST), that
may subsume the string contexts usage as well as other things like Terraform
interpolation  -->

<!-- tangentially, effects ~= recursive Nix? -->

[nix-lang]: https://gist.github.com/edolstra/29ce9d8ea399b703a7023073b0dbc00d
[nix-flakes]: https://nixos.wiki/wiki/Flakes
[nickel-effects]: https://github.com/tweag/nickel/issues/85
