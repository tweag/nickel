---
feature: nix-nickel
start-date: 2022-01-12
author: Yann Hamdaoui
---

# Nix-Nickel

The goal of the present RFC is to lay out a compelling plan to integrate Nix and
Nickel. We want to use Nickel as a front-end language for writing Nix
expressions in a seamless way -- or, at least, such that there is no aspect in
which Nickel is unreasonably worse than Nix expressions, and is hopefully better in others.

This document is structured as follows:

1. [Motivation](#motivation): exposes the motivation and the scope of this RFC,
   and gives concrete examples of what we want to achieve.
2. [Challenges](#challenges): identifies the technical challenges, as of today,
   raised by the goals defined in 1.
3. [Proposal](#proposal): gives concrete proposals to overcome those challenges
   and achieve the goals of 1.

## Motivation

Nickel is a general-purpose configuration language. Yet, Nix is one very special
use-case. Nix has been the main motivation for creating Nickel in the first
place. Now that Nickel has been released, the next step is to offer a compelling
experience for using Nickel in place of Nix as a language to write packages,
flakes and modules.

### Constraints

Because Nix is a distinct project from Nickel which has existed for quite
some time now, we need operate under the following constraints:

- _Do not require unreasonable changes to Nix itself_. While it's probably
    illusory to avoid any change in Nix at all, we must strive to keep them
    reasonably small and undisturbing for regular Nix users, at least in the
    middle term.
- _Implementable in a reasonable time, if possible incrementally_. This RFC aims
    at proposing a solid vision for the future and not a temporary work-around.
    Still, given the finite bandwidth of the Nickel team, the solution should be
    implementable in limited time (in the order of magnitude months, up to half
    a year). If possible, the plan should have intermediate milestones enabling
    new features in a incremental way.
- _Backward-compatible_. Related to the previous point, we will realistically
    need to be able to leverage existing Nix code (especially from Nixpkgs) in
    some way: otherwise, even in the (unlikely) best case scenario where
    everybody suddenly switches to Nickel right away, it'll take up a very long
    time to migrate everything.
- _Do not lock ourselves in the current Nixpkgs architecture_. Nixpkgs had
    made a number of design choices that are, in retrospective, not optimal
    (stdenv, describing packages as functions instead of data, etc.). While we
    have to keep a form of backward-compatibility, we want to do so while
    avoiding to tie ourselves to the current design and implementation. In other
    words, this document must propose a solution that stay compatible with
    radically departing from those choices.

### Scope

Nix expressions are used to several related but different ends, which come with
varying goals and constraints:

- Nixpkgs: derivations and packages in the original style.
- Flakes: new format for stand-alone, decentralized and composable packages.
- NixOS modules: system configuration.

We aims at handling all those cases in the long term, but the scope of such an
undertaking appears too large for a single RFC. We decide to focus on the first
item of the list: writing derivations and Nixpkgs-style packages. Indeed, a
derivation is the foundational concept on which the whole Nix model is based.
It's thus hard to imagine having a good story for using Nickel for NixOS or for
flakes without having one for derivations. It's frequent to use derivations as a
part of e.g. a NixOS module:

```nix
systemd.myUnit.UnitConfig.ExecStart = pkgs.writeScript "foo" "..."
```

Using `pkgs.writeScript` in an hypothetical NixOS Nickel module implies to know
how to generate derivations.

Derivations are in some sense "lower-level" than the other Nix concepts, which
build on it. It makes sense to tackle derivations first. From there, Nixpkgs is
probably the thinnest layer over derivations, compared to flake or the NixOS
module system.

## Nix-Nickel fantasised

Examples of using Nickel for Nix, in practice. What we want.

## Challenges

### Interaction with Nixpkgs

There a bunch of actions that require leveraging Nixpkgs:

- **USE**: Using a package from Nixpkgs, for example as a dependency
- **LIB**: Use one of the myriad of helpers from Nixpkgs: `mkShell`,
    `mkDerivation`, `writeText`, etc.
- **OVERRIDE**: Use a package from Nixpkgs but overriding some of its parameters

**USE** could be done in a simple way, by just passing around derivations from
Nix to Nickel. Derivations are just fully evaluated data, that can be encoded as
e.g. JSON.

**LIB** is more involved. Some of those helpers take either functions or lazy
data as parameters, and an interface (in the form of a FFI) between Nix and
Nickel would needs to handle those back and forth at the boundary of the two
languages transparently.

**OVERRIDE** is in practice technically the same issue as **LIB**, since it
amounts to calling Nix functions `override`, `overrideAttrs`, etc. That is,
solving **LIB** automatically solves **OVERRIDE**.

### String contexts

[String
contexts](https://shealevy.com/blog/2018/08/05/understanding-nixs-string-context/)
are a useful feature of Nix. In a Nix program, each string carries a context
recording which other variables it depends on at runtime, via string
interpolation. Nix can then automatically determine the dependencies of a
derivation.

## Proposal

### Interaction

As underlined in [Interaction with Nixpkgs](#interaction-with-nixpkgs), an
FFI-like interaction would imply transparent bidirectional communication between
Nix and Nickel. This is made very hard by the nature of both languages, which
are lazy and functional, implying that a Nix function called from Nickel may
incur calls back to Nickel arbitrarily (and vice-versa) to apply function or to
evaluate thunks. Not only this looks complex, but maintaining reasonable
efficiency seems to be a challenge too.

Another possibility is to have everything evaluate in only one world, either Nix
and Nickel. Practically, we could either compile the Nix code to Nickel, or the
Nickel code to Nix, and run everything there. Compiling Nickel to Nix and run
everything on the Nix side would have the advantage of leveraging all the Nix
primitives and machinery freely. However, this solution simply nullifies a lot
of the value proposition of Nickel. Compiling Nickel to Nix is not trivial, and
would loose the native support for contracts (and thus good error reporting),
overriding, and so on, going back to potential degraded performance and bad
error messages.

On the other hand, compiling Nix to Nickel appears simpler on paper. Nickel is
close to being a superset of Nix (the only non-trivial missing features being
the `with` construct and string contexts). This would preserve the Nickel
advances, and we could leverage directly any non-builtin Nix function, including
all the Nixpkgs library functions.

The possible drawbacks of this approach:
  - Performance
  - On the fly, or not
  - dynamic imports and typechecking

We will also need to reimplement Nix builtins in Nickel (the compat layer). This
is an interesting milestone, because even without the compiler, this will allow
to write derivation in Pure Nickel.

### String Context

- Overloaded string constructors.
  * Have a specific string delimiter `nix%"foo %{pkgs.hello}"%`
  * Have a corresponding contract or builtin to enforce Nix strings everywhere.
  * Use those in Nickel derivations. That means people will have errors with
    standard strings.

The question is how to make this process not Nix-specific but more general. Look
at g-expr in Guix (a bit of the same idea in the end?). Should the extension be
specified in Nickel itself, or as an interpreter plugin?
