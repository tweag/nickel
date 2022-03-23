---
slug: introduction
---

# The Nickel User Manual

Nickel is a generic configuration language.

Its purpose is to automate the generation of static configuration files - think
JSON, YAML, XML, or your favorite data representation language - that are then
fed to another system. It is designed to have a simple, well-understood core: it
is in essence JSON with functions.

Nickel's salient traits are:

- **Lightweight**: Nickel is easy to embed. An interpreter should be simple to
    implement. The reference interpreter can be called from many programming
    languages.
- **Composable code**: the basic building blocks for computing are functions.
    They are first-class citizens, which can be passed around, called and
    composed.
- **Composable data**: the basic building blocks for data are records
    (called *objects* in JSON). In Nickel, records can be merged at will,
    including associated metadata (documentation, default values, type
    contracts, etc).
- **Typed, but only when it helps**: static types improve code quality, serve as
    documentation and eliminate bugs early. But application-specific
    self-contained code will always evaluate to the same value, so type errors
    will show up at runtime anyway. Some JSON is hard to type. There, types are
    only a burden. Whereas reusable code - that is, *functions* - is evaluated
    on potentially infinitely many different inputs, and is impossible to test
    exhaustively. There, types are precious. Nickel has types, but you get to
    choose when you want it or not, and it handles safely the interaction between
    the typed and the untyped world.
- **Design by contract**: complementary to the type system, contracts are
    a principled approach to checking assertions. The interpreter automatically
    inserts assertions at the boundary between typed and untyped code. Nickel
    lets users add arbitrary assertions of their own and easily understand why
    when assertions fail.

The motto guiding Nickel's design is:
> Great defaults, design for extensibility

There should be a standard, clear path for common things. There should be no
arbitrary restrictions that limit what you can do you the one day you need to go
beyond.

## Use cases

Nickel is a good fit in any situation where you need to generate a complex
configuration, be it for a single app, a machine, whole infrastructure, or a
build system.

The motivating use cases are in particular:
- The [Nix package manager](https://nixos.org/): Nix is a declarative package
    manager using its own language for specifying packages. Nickel is an
    evolution of the Nix language, while trying to overcome some of its
    limitations.
- Infrastructure as code: infrastructure is becoming increasingly complex,
    requiring a rigorous approach to deployment, modification and configuration.
    This is where a declarative approach also shines, as adopted by
    [Terraform](https://www.terraform.io/),
    [NixOps](https://github.com/NixOS/nixops) or
    [Kubernetes](https://kubernetes.io/), all requiring potentially complex
    generation of configuration.
- Build systems: build systems (like [Bazel](https://bazel.build/)) need
    a specification of the dependency graph.

## Getting started

To get a Nickel binary working and run you first program, please follow the
[Getting Started](https://nickel-lang.org/getting-started) section of the
website.

## Current state and roadmap

Nickel has been released in version `0.1`. This version should be functional, it
is intended to gather feedback and real-life testing. Nickel `0.1` isn't intended
to be used in production. The next steps we plan to work on are:

- Nix integration: being able to seamlessly use Nickel to write shells, packages
  and NixOS modules.
- Custom merge functions and priorities (second part of the
  [overriding proposal](https://github.com/tweag/nickel/blob/9fd6e436c0db8f101d4eb26cf97c4993357a7c38/rfcs/001-overriding.md))
- Performance improvements

## Content

This document is a detailed documentation on the main aspects of the language.
It is composed of the following sections:

1. [Syntax](./syntax.md)
1. [Merging](./merging.md)
1. [Correctness](./correctness.md)
1. [Contracts](./contracts.md)
1. [Typing](./typing.md)
