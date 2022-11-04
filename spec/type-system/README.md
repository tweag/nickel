# Type system specification

**Warning**: this is ongoing work.

This directory contains a specification in
[OTT](https://github.com/ott-lang/ott) of the type system of Nickel. It contains
a declarative type system and a declarative type-inference specification. See
[RFC004](../../rfcs/004-typechecking.md) for more context.

## Build

Requires [Nix](https://nixos.org) and Make.

- Run `nix develop` to be dropped in a shell with all the required dependencies
- Run `make` to compile the OTT specification to latex and then to PDF.

## Notes

The notes subdirectory contains various informal notes, examples or anything
that is deemed good to record but not part of the specification per se.
