---
feature: nix-nickel
start-date: 2022-01-12
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

Because Nix is a distinct project from Nickel which has existed for quite
some time now, we need operate under the following constraints:

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

- Nixpkgs: derivations and packages in the original style.
- Flakes: new format for decentralized and composable packages.
- NixOS modules: NixOS system configuration.

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
the thinnest layer over derivations of the three, and already open a lot of
possibilities, so we choose to include it as well.

## Nix-Nickel fantasised

Examples of using Nickel for Nix, in practice. What we want.

## Challenges

### Interaction with Nixpkgs

There a bunch of actions that require leveraging Nixpkgs:

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
Nickel sounds like a degradation of developers' quality of life that is hard to
justify.

## Proposal

### Interaction

As underlined in [Interaction with Nixpkgs](#interaction-with-nixpkgs), an
FFI-like interaction would require a transparent bidirectional communication
between Nix and Nickel. This is made very hard by the nature of both languages,
which are lazy and functional, meaning that a Nix function called from Nickel
may incur arbitrary calls back to Nickel (and vice-versa) to apply a function or
to evaluate a thunk. Not only does this looks complex, but maintaining reasonable
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
