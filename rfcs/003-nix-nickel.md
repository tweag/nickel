---
feature: nix-nickel
start-date: 2022-01-12
author: Yann Hamdaoui
---

# Nix-Nickel

The goal of the present RFC is to elaborate a compelling plan to integrate Nix
and Nickel. We want to use Nickel as a front-end language for writing Nix
expressions in a seamless way -- or, at least, such that there is no aspect in
which Nickel is unreasonably worse than Nix expressions. Hopefully, Nickel will
hopefully fare better than Nix in other aspects.

This document is structured as follows:

1. [Motivation](#motivation): exposes the motivation for and the scope of this
   RFC, and gives concrete examples of what we want to achieve.
2. [Challenges](#challenges): identifies the technical challenges, as of today,
   raised by the goals defined in 1.
3. [Proposal](#proposal): presents concrete proposals to overcome those challenges
   and achieve the goals.

## Motivation

Nickel is a general-purpose configuration language. Yet, Nix is one very special
use-case. Nix was one of the motivation for creating Nickel in the first place.
Now that Nickel has been released, and the core language has been relatively
stabilized, the next step is to offer a compelling experience for using Nickel
in place of Nix as a language to write packages, flakes and modules.

### Constraints

Because Nix is a distinct project from Nickel which has existed for quite
some time now, we need operate under the following constraints:

- _Do not require unreasonable changes to Nix itself_. While it's probably
    illusory to avoid any change in Nix at all, we must strive to choose a
    solution that keeps them reasonable and as undisturbing as possible for
    regular Nix users, at least in the mid term.
- _Implementable in a reasonable time, if possible incrementally_. This RFC aims
    at proposing a solid vision and not a temporary work-around. That being
    said, the development bandwidth of the Nickel team is finite, and the
    solution should be reasonably implementable in the order of magnitude that
    stay under a year(??).
- _Backward-compatible_. Related to the previous point, we will realistically
    need to be able to leverage existing Nix code in some way: otherwise, even
    in the (unlikely) best case scenario where everybody suddenly switches to
    Nickel right away, it'll take up a very long time to migrate everything.
- _Do not lock ourselves in the current Nix/Nixpkgs architecture_. Nixpkgs had
    made a number of design choices that are, in retrospective, not optimal
    (stdenv, describing packages as functions instead of data, etc.). While we
    have to keep a form of backward-compatibility, we want to also avoid locking
    ourselves by instilling some of those questionable design choices into this
    proposal. In other word, this document must propose a solution that stay
    compatible with radically departing from those choices. 

### Scope

There a several way of using Nix, and in particular in writing Nix expressions:

- Nixpkgs: derivations and packages in the original style
- Flakes: Stand-alone, composable packages
- NixOS modules: system configuration modules

Packages: because the other aspects of Nix heavily rely on derivations, anyway.
Thus, derivations are foundational: it's hard to imagine having a good story for
the others before having one for derivations. What's more, the derivation story
is just lower-level, and simpler. It's frequent to write derivations as a part
of a module, even in something as simple as

```nix
systemd.myUnit.UnitConfig.ExecStart = pkgs.writeScript "foo" "..."
```

## Nix-Nickel fantasised 

Examples of using Nickel for Nix, in practice. What we would want.

## Challenges

### Interactions

Leveraging Nixpkgs and Nixosmodules (probably) require a bidirectional
communication between the two.

### String contexts

### Modules

## Proposal

### Interaction

Compile Nix to Nickel.

### String Context
