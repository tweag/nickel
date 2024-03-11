# Polymorphism

This example shows the usage of (statically typed) polymorphism in Nickel
through `forall`. Polymorphic functions are the ones operating on generic values.
Polymorphic types can be nested in Nickel, although it is rarely needed. Note
that they must always be written explicitly: the typechecker never infers
polymorphic types for you.

## Run

```console
nickel eval polymorphism.ncl
```

## Playground

Try to swap some type variables, or to mess with the definition of the functions
to see the program failing to typecheck.
