---
feature: nix-nickel
start-date: 2022-01-12
author: Yann Hamdaoui
---

Nix-Nickel
==========

The goal of the present RFC is to lay down a compelling and complete plan to
integrate Nix and Nickel. More precisely, the goal is to be able to use Nickel
as a front-end language for writing Nix expressions in a seamless way -- or, a
minima, that there is no aspect in which using Nickel instead of Nix expressions
is unreasonably worse, while Nickel will hopefully fare better than Nix in
others.

This document is structured as follows:

1. [Motivation](#motivation): exposes the central motivations and gives very
   concrete example of what we want to achieve.
2. [Challenges](#challenges): identifies the technical challenges, as of today,
   which arise when trying to achieve the goals.
3. [Proposal](#proposal): a concrete proposal to overcome the challenges and
   achieve the goals.

# Motivation

Nickel is a general-purpose configuration language. Yet, Nix is one particularly
important target, as it is one of the motivation for creating Nickel in the
first place. Now that the first release is out and the core language has been
stabilized, the next step is to offer a compelling experience for using Nickel
in place of Nix as a language to write packages, flakes and modules.

## Constraints

- do not require unreasonable changes to Nix itself.
- implementable in a reasonable time, if possible incrementally.

## Scope

Packages, flakes, modules?

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

# Challenges

## Interactions

Leveraging Nixpkgs and Nixosmodules (probably) require a bidirectional
communication between the two.

## String contexts

## Modules

# Proposal

## Interaction

Compile Nix to Nickel.

## String Context
