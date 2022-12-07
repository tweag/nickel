---
feature: typechecking
start-date: 2022-05-16
author: Yann Hamdaoui
---

# Typechecking

The goals of this RFC are:

1. Identify the main shortcomings of the current implementation of typechecking,
   in particular with respect to the interaction between typed and untyped code.
2. Write a proper formalization of a type system and companion type inference
   algorithm which overcome those limitations as much as possible. If this
   proves too hard, a proper definition and formalization of the current type
   system would already be a positive outcome.

The motivation for the first step is self-explanatory: a more expressive type
system directly translates to users being able to write more programs (or clearer
versions of the same program, without superfluous annotations).

The second step is motivated by improving the experience of extending the type
system in the future and maintaining the Nickel codebase. We already hit edge
cases that led to unsatisfying [ad-hoc
fixes](https://github.com/tweag/nickel/pull/586). The current implementation
interleaves a number of different phases, which makes it harder to get into and
to modify. Finally, a clean and well-designed specification often leads to a
simpler implementation, by removing accumulations of ad-hoc treatments that
become subsumed by a generic case.

## Background

There is a substantial literature on type systems for programming languages, as
well as implementations, some of them both cutting-edge and of industrial
strength. For the purely static part, the ML languages family and their cousins
(Haskell, OCaml, Scala, Purescript, etc.) has proved over time to be a solid and
expressive foundation. The role of the static type system of Nickel is to be
able to type various generic functions operating on primitive types, and doing
so doesn't seem to require new developments or very fancy types. The current
implementation, which supports polymorphism and row polymorphism, appears to
already cover most cases.

However, the co-existence of statically typed code and dynamically typed code
makes Nickel different from most of the aforementioned inspirations. Nickel is
indeed a gradually typed language.

Note that there are different flavours of gradual typing. A cornerstone of
_gradual type systems_ derived from the original work of Siek and Taha[^1] is to
statically accept potentially unsafe conversions from and to the dynamic type
`Dyn`, and more complex types with `Dyn` inside like converting `Dyn -> Num` to
`Num -> Num`. Such compatible types are said to be _consistent_ with each
others. These implicit conversions may or may not be guarded at runtime by a
check (_sound_ vs _unsound_ gradual typing).

```nickel
# If Nickel was gradually typed, this would be accepted
{
  add : Num -> Num -> Num = fun x y => x + y,
  mixed : Dyn -> Num -> Num = fun x y => add x (y + 1),
}
```

In Nickel, such implicit conversions are purposely not supported. Running this
examples gives:

```text
error: incompatible types
  ┌─ repl-input-0:3:46
  │
3 │   mixed : Dyn -> Num -> Num = fun x y => add x (y + 1),
  │                                              ^ this expression
  │
  = The type of the expression was expected to be `Num`
  = The type of the expression was inferred to be `Dyn`
  = These types are not compatible
```

A second — and directly related — peculiarity of Nickel are contract
annotations. Gradually typed languages _à la Siek and Taha_ usually use dynamic
checks called _casts_ to validate the implicit conversions at runtime, but those
casts are rarely part of the source language and rather an implementation
device. Contract annotations are in some way a first-class version of the
implicit casts of gradual typing. Thus, it is possible to make the previous
example work in Nickel by manually adding a contract annotation which indicates
what type the user expects the expression to be:

```nickel
{
  add : Num -> Num -> Num = fun x y => x + y,
  mixed : Dyn -> Num -> Num = fun x y => add (x | Num) (y + 1),
}
```

TypedRacket is an example of a gradually typed language that departs from the
original design and which requires explicit contract annotations, as Nickel
does.

### Summary

The important dimensions of typing in Nickel are:

- Statically typed code is checked using a standard ML-like/SystemF system with
  polymorphism and row polymorphism
- Nickel is stricter than a vanilla gradual type system, in that it doesn't
  allow implicit casts from and to the dynamic type.
- Nickel has contract annotations, which can be used to write type casts
  explicitly. They incur a runtime check.

## Motivation

This section attempts to motivate this RFC practically: what are programs which
are natural write and that we expect to be accepted by the typechecker, but
aren't currently?

### The dynamic type

The dynamic type acts as a top type statically (with a caveat though[^2]), like
`Any` or `Object` in some languages. Although casts from the dynamic type are
unsafe in general, cast _to_ the dynamic type are safe. But the typechecker
currently isn't smart enough and requires explicit annotations:

```nickel
# rejected because some_data has type `{script: Str, vars: Array Str}`, which is
# not equal to `Dyn`
{
  serialized : Array Str =
    let some_data = {script = "echo ${hello}", vars = ["hello"] } in
    let other_data = ["one", "two", "three"] in
    [
      builtin.serialize `Json some_data,
      builtin.serialize `Yaml other_data
    ]
}
```

To make it work, the user needs to explicitly add missing `Dyn` contract
annotations:

```nickel
{
  serialized : Arry Str =
    let some_data = {script = "echo ${hello}", vars = ["hello"] } in
    let other_data = ["one", "two", "three"] in
    [
      builtin.serialize `Json (some_data | Dyn),
      builtin.serialize `Yaml (other_data | Dyn)
    ]
}
```

### Dyn versus forall

Why do we need `Dyn` inside typed code at all? Untyped values are given the type
`Dyn`, but this is a different matter. This RFC is concerned with functions
_consuming_ values of type `Dyn`. In statically typed code, we can already
express operating over generic values using polymorphism, as in the definition
of `head`:

```nickel
head : forall a. Array a -> a
```

However, well-typed polymorphic functions are
[_parametric_](https://en.wikipedia.org/wiki/Parametricity). Concretely, they
can't inspect their polymorphic arguments. The contract system also enforces
this invariant dynamically. For example, the following function is rejected by
the contract system:

```nickel
let fake_id
  | forall a. a -> a
  = fun x => if builtin.is_num x then x + 1 else x in
fake_id 10
```

**Remark**: currently (as of version 0.2.1) this just returns false, but this is
a bug, see [#727](https://github.com/tweag/nickel/issues/727).

Any typed function that needs to inspect its argument needs to use `Dyn`, or at
least a less general type than `a`. A typical example is `array.elem`, which is
of type `Dyn -> Array Dyn -> Bool`, because it compares elements. This is a
source of `Dyn` inside typed functions:

```text
nickel>
let has_one : Bool =
  let l = [1, 2, 4, 5] in
  array.elem 1 l

error: incompatible types
  ┌─ repl-input-7:3:14
  │
3 │   array.elem 1 l
  │              ^ this expression
  │
  = The type of the expression was expected to be `Dyn`
  = The type of the expression was inferred to be `Num`
  = These types are not compatible
```

Indeed, we need to insert explicit casts:

```text
nickel>
let has_one : Bool =
  let l = [1, 2, 4, 5] in
  array.elem (1 | Dyn) (l | Array Dyn)
nickel> has_one
true
```

The following are two real life examples taken from the [Nixpkgs list
library](https://github.com/NixOS/nixpkgs/blob/09f6e893c8b0eacb93a2062cd7ebe32a95642c9b/lib/lists.nix),
converted to Nickel and statically typed. They both use `array.elem`:

```nickel
subtractLists: forall a. Array a -> Array a -> Array a
  | doc m%"
      Subtracts list 'e' from another list. O(nm) complexity.
     Example:
       subtractLists [ 3, 2 ] [ 1, 2, 3, 4, 5, 3 ]
       >> [ 1, 4, 5 ]
  "%
  = fun e => array.filter (fun x => !(array.elem (x | Dyn) (e | Array Dyn))),

mutuallyExclusive: forall a. Array a -> Array a -> Bool
  | doc m%"
      Test if two lists have no common element.
     It should be slightly more efficient than (intersectLists a b == [])
  "%
  = fun a b =>
    array.length a == 0
    || !(any (fun x => array.elem (x | Dyn) (a | Array Dyn)) b),
```

### Flow-sensitive typing

Another possible future use-case is [flow-sensitive
typing](https://en.wikipedia.org/wiki/Flow-sensitive_typing), as in e.g.
TypeScript. If we are to implement such a feature in Nickel, taking an argument
of type `Dyn` and then type-casing (using `builtin.typeof` or `builtin.is_xxx`)
would make sense inside statically typed code, naturally producing functions
that consume values of type `Dyn`. Here is another example from the Nixpkgs list
library which could use flow-sensitive typing:

```nickel
flatten: Dyn -> Array Dyn
  | doc m%"
      Flatten the argument into a single list, that is, nested lists are
     spliced into the top-level lists.
     Example:
       flatten [1, [2, [3], 4], 5]
       >> [1, 2, 3, 4, 5]
       flatten 1
       >> [1]
  "%
  = fun x =>
     if builtin.is_array x then
       concatMap (fun y => flatten y) (x | Array Dyn)
     else
       [x],
```

### Dictionaries

In essence, an upcast can be seen as _forgetting_ part of the type information.
The most drastic loss of information is casting to `Dyn`: we pretty much forget
everything about the original type of the value.

There are other useful loss of information. One concerns dictionary types `{_:
T}`. In practice, records serve several purposes in Nickel:

- A key-value mapping with a statically known structure. This is usually
  the case for configurations: one knows in advance what key will appear in the
  final value and for each key, the type of allowed values.
- A key-value mapping with a dynamic structure. Keys are added
  dynamically and depend on runtime values.

The first case is best modeled using record types. For example:

```nickel
{
  name = "virtal",
  version = "1.0.1",
  enabled = true,
} : {
  name : Str,
  version : Str,
  enabled : Bool,
}
```

But for records which fields are not statically known, record types are too
rigid. We use dictionary types instead:

```nickel
(let data = {ten = 10} in
data
|> record.insert "one" 1
|> record.insert "two" 2
|> record.insert "three" 3) : {_: Num}
```

Unfortunately, this example doesn't work as it is:

```text
error: incompatible types
  ┌─ repl-input-6:2:1
  │
2 │ data
  │ ^^^^ this expression
  │
  = The type of the expression was expected to be `{_: Num}`
  = The type of the expression was inferred to be `{ten: Num}`
  = These types are not compatible
```

Record literals are always inferred to have a precise record type (here, `{ten :
Num}`) which is thus different from the expected dictionary type `{_ : Num}`. A
special casing in the typechecker still makes this example pass with an
additional annotation:

```nickel
(let data : {_ : Num} = {ten = 10} in
data
|> record.insert "one" 1
|> record.insert "two" 2
|> record.insert "three" 3) : {_: Num}
```

Alas, this special case is fragile: it only works if we annotate directly the
record literal. If we use a variable as a proxy, inference is once again broken:

```nickel
nickel> (let x = {ten = 10} in
let data : {_ : Num} = x in
data
|> record.insert "one" 1
|> record.insert "two" 2
|> record.insert "three" 3) : {_: Num}

error: incompatible types
  ┌─ repl-input-8:2:24
  │
2 │ let data : {_ : Num} = x in
  │                        ^ this expression
  │
  = The type of the expression was expected to be `{_: Num}`
  = The type of the expression was inferred to be `{ten: Num}`
  = These types are not compatible
```

## Proposal

Given the precedent sections, we propose to add a subtyping relation generated
by `T <: Dyn` for all type `T` that is not a type variable, and `{l1 : T, ..,
ln : T} <: {_: T}` for the dictionaries use-case. Data structure are covariant,
and the arrow has the usual co/contra-variance.

### `Dyn` inference

As long as there is no `Dyn` involved by the type given by either the signature
of a function or the annotation of an argument, we want subtyping to not change
the current behavior. For the typechecker, this means following this guideline:

_Never infer a `Dyn` type implicitly. `Dyn`, and dictionary subtyping, must
always stem from an explicit annotation._

For example, the following program is expected to **fail** typechecking:

```nickel
let uni_pair : forall a. a -> a -> {fst: a, snd: a} = fun x y =>
  { fst = x, snd = y } in

uni_pair 1 "a" : _
```

We could infer the type `Dyn` for the instantiation of `a`. But this is
currently an error, and it should arguably stay this way. `Dyn` can make a lot
of such cases pass, only to manifest as a later type error – and probably a
disconcerting one involving a `Dyn` coming from nowhere – when the result of the
function is actually used. However, if one of the argument is explicitly of type
`Dyn`, then it is reasonable to allow the other one to be silently upcast:

```nickel
let foo : Dyn = 1 in
uni_pair foo 1 : _
```

Unfortunately, we don't have yet a good characterization of this as a property
of the declarative type system or declarative type inference. It is an informal
guideline, which is only reflected in the algorithmic type inference for now.

### Polymorphism and subtyping

The previous example shows an interesting aspect of combining subtyping and
polymorphism: if we instantiate type variables at the first type we see, as we
do currently, typechecking is sensitive to the order of arguments:

```nickel
# ok, a is instantiated to Dyn, and then 1 : Dyn
uni_pair foo 1 : _
# would fail, a is instantiated to Num, but foo is not of type Num
uni_pair 1 foo : _
```

This problem is dubbed the Tabby-first problem in [the review of bidirectional
typing of Dunfield and Krishnaswami](https://arxiv.org/abs/1908.05839).
Surprisingly, it doesn't seem to have been deeply studied (outside of the
subtyping relation induced by polymorphism). For subtyping induced by
polymorphism, there are different trade-offs: Dunfield and Krishnaswami
restricts their system to predicative polymorphism, which is then
order-independent [^3]. In exchange, the arrow type has the expected variance
(that is, eta-expansion preserves typeability). On the other hand, Haskell
deemed impredicative polymorphism to be more important and chose to abandon the
variance of arrows and other type constructors. Coupled with a so-called
QuickLook phase, Haskell is able to guess most common impredicative
instantiations.

We can't apply the Haskell approach naively, because in the presence of `Dyn`,
an impredicative instantiation is not the only possibility anymore, even if we
don't do deep instantiation or deep skolemization:

```nickel
let foo : Dyn = null in
let ids : Array (forall a. a -> a) = [fun x => x] in
let concat : forall b. Array b -> Array b -> Array b = (@) in

concat ids [null]
```

In the QuickLook setting, when we see the argument `ids`, we know that `b`
**must** be instantiated with `forall a. a -> a`. This is not true here, as `b`
could also be `Dyn` (and in this case, given the second argument, it should!).
We can't delay solving constraints involving polymorphic function types as we
could do with monomorphic types (in that case, just generating `?a >: forall a.
a -> a` at the first argument without solving right away): when applying a
function with type `?a`, we need to know if `?a` must be instantiated with an
arrow or if it has potential heading `forall`s which need to be instantiated.

We may extend the QuickLook phase to also cover `Dyn`, but this needs more
investigation. Another fix could be to make all type constructors invariant also
for the subtyping relation generated by `Dyn`, but that is quite restrictive in
practice (several examples in this document would need `Array T <: Array Dyn` in
order to get rid of the casts).

### Packing up

To sum up:

- We only want to infer `Dyn` because of an explicit type annotation.
- Polymorphism and subtyping interact in a non-trivial way. It is not obvious
   how to apply QuickLook in the presence of our proposed subtyping relation.

Given that impredicativity looks complex to combine with subtyping, and isn't
really a strong motivation for a language like Nickel, a reasonable choice is to
rather follow the path of Dunfield and Krishnaswami. However, the recent uproar
upon the removing of the subsumption rule for arrows in Haskell[^4] shows that
such things (variance of arrows with respect to polymorphism-induced subtyping)
are way easier and safer to add than to remove, from a user experience
perspective. In consequence, we propose to take a more restrictive approach
first, which is still open to either pursing one road or the other if there is
such a need later.

The final proposal is a system that:

- Has subtyping generated by the dynamic type and dictionaries.
- Support higher-rank polymorphism
- Is predicative
- Doesn't do deep skolemization/instantiation, or equivalently, in which type
  constructors are invariant with respect to subtyping induced by polymorphism

A declarative specification as well as a constraint-solving algorithm are
provided in this repository. See
[specs/type-system](../spec/type-system/README.md)

## Alternatives

### No subtyping

An obvious alternative is to abandon the idea of subtyping. From there we can
either choose to follow the QuickLook approach, the Dunfield and Krishnaswami's
one, or the restricted one described above (but without subtyping, of course).
We will have to put up with adding contract annotations for `Dyn` and
dictionaries.

### Dictionary-only subtyping

Another middle-ground could be to only keep the dictionary part of the subtyping
relation. Because it is then guarded by the `{_ : _}` type constructor, it
doesn't interact with polymorphism (in that a polymorphic type can never be a
subtype of anything). We lose the upcasts to `Dyn` but we still improve on the
current situation for dictionaries.

[^1]: [Gradual Typing for Functional Languages](http://scheme2006.cs.uchicago.edu/13-siek.pdf)
[^2]: `Dyn` is not greater than type variables (also referred as to
  existentials, skolem variables or rigid type variables), because that would
  break parametricity.
[^3]: [Complete and Easy Bidirectional Typechecking for Higher-Rank
  Polymorphism](https://arxiv.org/abs/1306.6032)
[^4]: [This reddit
  discussion](https://www.reddit.com/r/haskell/comments/ujpzx3/was_simplified_subsumption_worth_it_for_industry/)
  gather some reactions of Haskell users to the removal of arrow subsumption
