---
object: 1st standardization meeting
attending:
  - Arnaud Spiwack
  - Eelco Dolstra
  - Thépohane Huschmitt
  - Yann Hamdaoui
author: Yann Hamdaoui (yann.hamdaoui@tweag.io)
date: "Dec 7, 2021"
---

# Nickel Standardization Meeting #1

Attending:
- Arnaud Spiwack
- Eelco Dolstra
- Théophane Huschmitt
- ~~Rok Garbas~~
- Yann Hamdaoui

## Agenda

### Block let-binding
[#218](https://github.com/tweag/nickel/issues/218)

Should we have block let-binding? See issue for proposals. Related: should we
have a Haskell-like `where`?

**In-meeting notes**
We choose Nix-style but replacing `;` with `,` for consistency:

```
let
  foo = 1,
  bar = 2,
  baz = 3
in
...
```

Like this, the definition of a record and let-bindings are consistent.

### Top-level let-binding
[#95](https://github.com/tweag/nickel/issues/95)

**In-meeting notes**
Not discussed.

### Case convention
For now, we use camelCase for values and CamelCase for types and contracts. Is
it fine? Should we enforce this? What should be the convention?

**In-meeting notes**
Use the Rust naming convention: snake_case for values, CamelCase for
types/contracts.

### Allow dash in identifiers

We should allow the dash character `-` inside identifiers, even if it makes
parsing subtraction a bit more annoying, because package configurations may
exhibit a lot of fields with dashes in it. We can currently escape them with
quotes, but it's still much more common than arithmetic expressions in Nickel.
Although, requiring a space between subtraction operand is fine.

### Stdlib stabilization

**In-meeting notes**
No well scoped question here. To be discussed/done later.

### Function syntax
[#207](https://github.com/tweag/nickel/issues/207)

Current syntax: `nickel let f = fun x y z => body in`

**Alternatives**
- ~~Remove the `fun`~~ (makes the grammar ambiguous)
- Change for Haskell-style `let f = \x y z => body`
- Add ML-style `let f x y z = body in`. What about partial type annotations?

**In-meeting notes**
`fun` is fine. In favor of second Ocaml-style function definition `let f x y z =
...`. What about partial annotations (what is the semantics of `f (x :
Num) y z =` outside of a typed block) ? The problem is already there with or
without the OCaml-style function definitions. We should also have an Haskell-style annotation "by pieces" consistent with the let-block syntax:

```nickel
let f : Foo -> Bar,
    f x y = ...,
    g : Bar -> Stuff,
    g z x = ...,
in
```

### Type hole syntax
[#226](https://github.com/tweag/nickel/issues/226)

Currently, one has to write explicitly type signature to turn typechecking on:
`let foo : SomeType = ... in ...`

**Proposal**
`let foo : _ = ... in ...`. Each type hole would be substituted for a
unification variable at and resolvesd at typechecking. Unconstrained variables
are substituted for `Dyn`.

**Usages**
 - typing libraries:
 ```nickel
 {
   bar : Num -> Num = fun x => x + 1,
   foo : forall a. a -> -a = fun x => x,
 } : _
 ```
 (but beware of duplicated contract checks)
 - enable typechecking without having to know the top-level type. For debugging.
 ```
 let somethingWrong = ...
 // transform to
 let somethingWrong : _ = ...
 ```

**Question**
Should this generate a contract? Yes.

**In-meeting notes** In favor of typed holes.

Haskell uses the term and syntax of "typed holes" for
something a tad different (basic interactive programming). If we do so at some
point, we should use a different character like `?`. `_` for libraries may
duplicate contracts.  Nonetheless, we should treat this contract duplication
issue separately: the general issue of optimizing and eliding contracts.

### File extension
[#357](https://github.com/tweag/nickel/issues/357)

Currently `.ncl` seems to clash with other file formats.

**Proposals**
Should we have multiple extensions? Probably not (searchability).
 - `.nickel`
 - `.nkl`
 - `.nikl`
 - `.nicl`

**In-meeting notes**
No stake. Doesn't impose constraints on other aspects of the development. No real
objective criterion. We can decide this later.

### Merging with siblings
Once overriding lands, we may want to refer to fields that will only be defined
later.

Example:
```nickel
{foo = bar + 1} & {bar}

// same name
{foo = foo + 1} & {foo = 2}

// Encoding CUE if
let value = {
  protocol | <http, telnet>
           | default = `http,
} & {proxProtocol = protocol}
```

But it introduces dynamic scoping, which is bad.

**Proposal**
- add it nevertheless
- force the definition of an interface:
  ```nickel
  {foo = bar + 1, bar | Num} & {bar}
  // For more complex/dynamic interfaces
  let Interface = {bar | Num} in
  {foo = bar + 1, ..Interface}
  ```

**In-meeting notes**
In favor of NOT having dynamic scoping, and requiring explicit declaration of
interfaces in one way or another. We can also already currently put the merged
content in a subfield as in:

```nickel
let Interface = {bar | Num} in
{foo = params.bar + 1, params | #Interface}
```

Later, if mandated by e.g. NixOS modules, we could add the `..Interface` syntax
to "inline" one record inside the other:

```nickel
let Interface = {bar | Num} in
{foo = bar + 1, ..Interface}
```

For patching a field, one should either use a function or later (ab)use merge
functions:

```nickel
mapField "foo" (fun x => x +1) {foo = 1}

let add = fun x y => x + y in
({foo | merge add = 1} & {foo = 1})
```

### Types and custom contracts

General issue: lack of symmetry. Lack of clear separation. We have two syntactic worlds, type syntax, and
value syntax. We have the following transformations:

```
# : (Lbl -> Dyn -> Dyn \/ {_ : Dyn}) -> Type
open_contract(): Type -> (Lbl -> Dyn -> Dyn)

open_contract(#Foo) = Foo
open_contract(#(open_contract(T))) =  open_contract(T)
```

**Issues**
- `open_contract` is not accessible by the programmer. Example:
  ```nickel
  let Nullable = fun contract label value =>
  if value == null then value
  else contract label value in

  //cannot do
  foo | #Nullable (C1 & C2 & C3)
  ```
- `#` is verbose: should we remove it? But in current situation, it is nice to
  embed value syntax in the type syntax as in

  ```nickel
  foo | #(Nullable Port) = ...
  foo | #(GreaterThan 2) = ...
  foo | #(Nullable (AllOf [Port, Unreserverd])) = ...
  foo | #(Nullable {fst = )) = ...
  ```
- Contracts are bare values. It means we can't recognize them and apply special
  behavior.

  Example:
  ```nickel
  //Naive intersection via merging. Would be nice. Work for records but not for
  //Currently can only do "point-wise" val | C1 | C2 | C3
  let Ctr1 = C1 & C2 & C3
  ```

**Proposals**
- Make contracts definition special/enforce case.
- Add datatype syntax for contracts
- Add a way to extract contract from arbitrary type.

**In-meeting notes**
For contracts as bare values: contracts could be specific records with whatever
additional fields we want. But then contract-of-record can't be used as normal
values anymore (field projection, merging, and so on), which is sad. Another way
would be to have yet another metadata distinguishing contracts so that they can
be both special cased for specific operations and yet act as normal values
otherwise. Contract definition would have a specific syntax like `let contract
Foo = ...`.

`open_contracts()` should be available to the programmer.

But more generally, we should maybe merge type and values syntax. It is not
trivial but solve some if not all of our problems. Given an AST with mixed value
and type constructors, either the typechecker knows a symbol specifically, or it
doesn't and consider it as an opaque type. Morally, the parser would "place" hashes
automatically.

Example (doesn't make sense semantically, but to demonstrate parsing):
`GreaterThan 2 -> List Dyn -> (fun x => x + (1 -> 2))` would be equivalent to (in current syntax)
`#(GreaterThan 2) -> List Dyn -> #(fun x => x + (#1 -> #2).open_contract())`.
The return type `#(fun x ...)` is a custom contract and thus an opaque type from
the point of view of the typechecker.

This would solve the following issues:
- special casing of contract application to value
  ([#445](https://github.com/tweag/nickel/issues/445))
- nullable of several contracts, if in conjunction with defining merging for
  contracts as composition ([#461](https://github.com/tweag/nickel/issues/461)

We wouldn't need `open_contract` anymore, as we could use as-of-now types
directly as values.
