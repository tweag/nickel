# Typing in Nickel

## Introduction

Nickel takes an hybrid approach to typing: *gradual typing*. Gradual typing
enables to mix both static typing and dynamic typing.

Usually, static typing brings in important benefits for large codebases of
general-purpose programming languages, but the case of an interpreted
configuration language may appear less clear-cut.

Nevertheless, who has ever faced puzzling [dynamic type
errors](https://www.haskellforall.com/2021/01/dynamic-type-errors-lack-relevance.html)
may have felt the need for something better. Standard dynamic typing is prone to
error messages being unrelated to the actual issue while being located far from
the offending code. This is especially salient when working with functions,
which tend to delay type errors by passing around ill-formed values until they
eventually break evaluation somewhere else.

On the other hand, for pure configuration code, static typing is less useful.
First, a configuration is a terminating program run once on fixed inputs: here,
basic type errors will show up at runtime anyway. What's more, Nickel has a
powerful validation system, contracts, that can do the same job as types and
more.

The need for statically typing functions but not configuration code is the
motivation for gradual typing.

You should at least skim at this document first, but once you have even a
partial understanding of how typing works, you should look at the practical
guide [Type versus contracts: when to?](./types-vs-contracts.md) if what you
seek is knowing when and how to annotate your code.

## Typing modes

### Untyped by default

By default, Nickel code is dynamically typed.

Example:
```nickel
{
  name = "hello",
  version = "0.1.1",
  fullname =
    if builtins.isNum version then
      "hello-v#{strings.fromNum version}"
    else
      "hello-#{version}",
}
```

While dynamic typing is fine for configuration code, especially when checked
against a contract, we run into the bad error reporting issue once we are using
functions.

```
let filter = fun pred l =>
  lists.foldl (fun acc x => if pred x then acc @ [x] else acc) [] l in
filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
```

Result:
```
error: Type error
  ┌─ repl-input-11:2:32
  │
2 │   lists.foldl (fun acc x => if pred x then acc @ [x] else acc) [] l in
  │                                ^^^^^^ This expression has type Num, but Bool was expected
3 │ filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
  │                                                             - evaluated to this
  │
  = if
```

This example shows how dynamic typing delays type errors, making them harder to
diagnose. Here, `filter` is fine, but the error still points to inside its
implementation. The actual issue is that the caller provided an argument of the
wrong type: the function should return a boolean, but returns either the
original element or `null`. This is a tiny example, so debugging is still doable
here. In a real code base, the user (who probably wouldn't even be the author of
`filter` in practice) might have a harder time solving the issue from the error
report.

### Typed blocks

The `filter` example is the poster child for static typing. The typechecker will
catch the error early as the type expected by `filter` and the return type of
the filtering function passed as the argument don't match .

To call the typechecker to the rescue, use `:` to introduce a *type annotation*.
This annotation switches the typechecker on inside the annotated expression, be
it a variable definition, a record field or any expression via an inline
annotation. We will refer to such an annotated expression as a *statically typed
block*.

Examples:
```
// Let binding
let f : Num -> Bool = fun x => x % 2 == 0 in

// Record field
let r = {
  count : Num = 2354.45 * 4 + 100,
} in

// Inline
1 + ((if f 10 then 1 else 0) : Num)
```

Let us try on the filter example. We want the call to be inside the statically
typechecked block. The easiest way is to add an annotation at the top-level:

```nickel
(let filter = fun pred l =>
     lists.foldl (fun acc x => if pred x then acc @ [x] else acc) [] l in
filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]) : List Num
```

Result:
```
error: Incompatible types
  ┌─ repl-input-12:3:37
  │
3 │ filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]) : List Num
  │                                     ^ this expression
  │
  = The type of the expression was expected to be `Bool`
  = The type of the expression was inferred to be `Num`
  = These types are not compatible
```

This is already better! The error now points at the call site, and inside our
anonymous functions, telling us it is expected to return a boolean. What's more,
we just had to give the top-level annotation `List Num`. Nickel performs type
inference, so that you don't have to write the type for `filter`, the filtering
function nor the list.

**Take-away**: Nickel takes an hybrid approach to typing that mixes both static
typing and dynamic typing. The default is dynamic typing. The static typechecker
kicks in when using a type annotation `exp : Type`. This delimits a typed block.
Nickel has type inference, sparing you writing unnecessary type annotations.

## Type system

Let us now have a quick tour of the type system. The basic types are:

- `Dyn`: the dynamic type. This is the type given to most expressions outside of
  a typed block. A value of type `Dyn` can be pretty much anything.
- `Num`: the only number type. Currently implemented as a 64bits float.
- `Str`: a string.
- `Bool`: a boolean, that is either `true` or `false`.
<!-- - `Lbl`: a contract label. You usually don't need to use it or worry about it, -->
<!--     it is more of an internal thing.  -->

The following type constructors are available:

- **List**: `List T`. A list of elements of type `T`. When no `T` is specified, `List`
  alone is an alias for `List Dyn`.

  Example:
  ```nickel
  let x : List (List Num) = [[1,2], [3,4]] in
  lists.flatten x : List Num
  ```
- **Record**: `{field1: T1, .., fieldn: Tn}`. A record whose field
  names are known statically as `field1`, .., `fieldn`, respectively of type
  `T1`, .., `Tn`.

  Example:
  ```nickel
  let pair : {fst: Num, snd: Str} = {fst = 1, snd = "a"} in
  pair.fst : Num
  ```
- **Dynamic record**: `{_: T}`. A record whose field
  names are statically unknown but are all of type `T`.  Typically used to model
  dictionaries.

  Example:
  ```nickel
  let occurences : {_: Num} = {a = 1, b = 3, c = 0} in
  records.map (fun char count => count+1) occurences : {_ : Num}
  ```
- **Enum**: `<tag1, .., tagn>`: an enumeration comprised of alternatives `tag1`,
  .., `tagn`. An enumeration literal is prefixed with a backtick and serialized
  as a string. It is useful to encode finite alternatives. The advantage over
  strings is that the typechecker handles them more finely: it is able to detect
  incomplete matches, for example.

  Example:
  ```nickel
  let protocol : <http, ftp, sftp> = `http in
  (switch {
    `http => 1,
    `ftp => 2,
    `sftp => 3
  } protocol) : Num
  ```
- **Arrow (function)**: `S -> T`. A function taking arguments of type `S` and returning a value of
  type `T`. For multi-parameters functions, just iterate the arrow constructor.

  Example:
  ```nickel
  {
    incr : Num -> Num = fun x => x + 1,
    mkPath : Str -> Str -> Str -> Str = fun basepath filename ext =>
      "#{basepath}/#{filename}.#{ext}",
  }
  ```

### Polymorphism

#### Plain polymorphism

Usually, a function like `filter` would be defined in a library. In this case,
it is good practice to write an explicit type annotations, if only to provide an
explicit interface for the consumers of the library. What should be the type
annotation for `filter`?

In our initial `filter` example, we are filtering on a list of numbers. But the
code of `filter` is agnostic with respect to the type of elements of the list.
That is, `filter` is *generic*. Genericity is expressed in Nickel through
*polymorphism*.  Polymorphic parameters are introduced by the keyword `forall`,
and can later be substituted for any concrete type. Here is our polymorphic
type annotation:

```nickel
{
  filter : forall a. (a-> Bool) -> List a -> List a = ...,
}
```

Now, filter can be used on numbers as in our initial example, but on strings as
well:

```nickel
{
  foo : List Str = filter (fun s => strings.length s > 2) ["a","ab","abcd"],
  bar : List Num = filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6],
}
```

**Type inference and polymorphism**
the type inferred by the typechecker is,
guessed from the application of `filter` following its definition. If we try to
use `filter` on a list of elements with a different type, we get in trouble:


In our initial `filter` example, we are using `filter` on a list of booleans, so
we could do this:

```nickel
{
  filter : (Num -> Bool) -> List Num -> List Num = ...,
}
```

But this type is limiting. For example, we couldn't do `filter (fun s =>
strings.length s > 2) ["a","ab","abcd"]`.

```nickel
(let filter = ... in
let result = filter (fun x => x % 2 == 0) [1,2,3,4,5,6] in
let dummy = filter (fun s => strings.length s > 2) ["a","ab","abcd"] in
result) : List Num
```

Result:
```
error: Incompatible types
  ┌─ repl-input-35:2:37
  │
2 │ let dummy = filter (fun s => strings.length s > 2) ["a","ab","abcd"] in
  │                                             ^ this expression
  │
  = The type of the expression was expected to be `Str`
  = The type of the expression was inferred to be `Num`
  = These types are not compatible
```

That's too bad, because the code of `filter` is in fact agnostic with respect to
the type of elements of the list. That is, `filter` is *generic*.  This can be
expressed in Nickel with so-called *parametric polymorphism*. This is really
just a pedantic name for some flavour of generics. Generic parameters are
introduced by the keyword `forall`, and can then be used as any other type.  We
can now fix our example:

```nickel
(let filter : forall a. (a -> Bool) -> List a -> List a in
let result = filter (fun x => x % 2 == 0) [1,2,3,4,5,6] in
let dummy = filter (fun s => strings.length s > 2) ["a","ab","abcd"] in
result) : List Num
```

Result:
```
[2,4,6]
```

And now it works! `forall a. (a -> Bool) -> List a -> List a` means it is OK to
substitute `a` for any type. You can use as many generic parameters as you need:

```nickel
let fst : forall a b. a -> b -> a = fun x y => x in
let snd : forall a b. a -> b -> b = fun x y => y in
{ n = fst 1 "a", s = snd 1 "a" } : {n: Num, s: Str}
```

Or even nest them:

```nickel
let higherRankId : forall a. (forall b. b -> b) -> a -> a
  = fun id x => id x in
let id : forall a. a -> a
  = fun x => x in
higherRankId id 0 : Num
```

#### Type inference and polymorphism

If we go back to our first example of the statically typed `filter` without the
polymorphic annotation and try to add a call to `filter` on a list of strings,
the typechecker surprisingly rejects our code:

```nickel
(let filter = ... in
let result = filter (fun x => x % 2 == 0) [1,2,3,4,5,6] in
let dummy = filter (fun s => strings.length s > 2) ["a","ab","abcd"] in
result) : List Num
```

Result:
```
error: Incompatible types
  ┌─ repl-input-35:2:37
  │
2 │ let dummy = filter (fun s => strings.length s > 2) ["a","ab","abcd"] in
  │                                             ^ this expression
  │
  = The type of the expression was expected to be `Str`
  = The type of the expression was inferred to be `Num`
  = These types are not compatible
```

The reason is that without an explicit polymorphic annotation, the typechecker
will always infer non-polymorphic types. Here, `filter` is given the type `(Num
-> Bool) -> List Num -> List Num`, guessed from the application in the right
hand side of `result`. **If you need a polymorphic type, you have to write an
explicit annotation**.

**Note**:
if you are a more type-inclined reader, you may wonder why the typechecker is
not capable of inferring a polymorphic type for `filter` by itself. Indeed,
[Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
type-inference can precisely infer heading `foralls`, such that the previous
rejected example would be accepted. We chose to abandon this so-called automatic
generalization, because it makes other aspects of the type system and type
inference more complex or limited. Requiring annotation of polymorphic functions
seems like a good practice and an acceptable price to pay in exchange of making
the type system simpler and more easily extensible, in a non type-heavy
configuration language like Nickel.

#### Row polymorphism

In a configuration language, you will often find yourself handling records of
various kinds. In a simple type system, you can hit the following issue:

```nickel
(let addTotal: {total: Num} -> {total: Num} -> Num
  = fun r1 r2 => r1.total + r2.total in
let r1 = {jan = 200, feb = 300, march = 10, total = jan + feb} in
let r2 = {aug = 50, sept = 20, total = aug + sept} in
let r3 = {may = 1300, june = 400, total = may + june} in
{
  partial1 = addTotal r1 r2,
  partial2 = addTotal r2 r3,
}) : {partial1: Num, partial2: Num}
```

```
error: Type error: extra row `sept`
  ┌─ repl-input-40:8:23
  │
8 │   partial2 = addTotal r2 r3,
  │                       ^^ this expression
  │
  = The type of the expression was expected to be `{total: Num}`, which does not contain the field `sept`
  = The type of the expression was inferred to be `{total: Num, sept: Num, aug: Num}`, which contains the extra field `sept`
```

The problem here is that for this code to run fine, the requirement of
`addTotal` should be that both arguments have a field `total: Num`, but could
very well have other fields, for all we care. Unfortunately, we don't know right
now how to express this constraint. The current annotation is too restrictive,
because it imposes that arguments have exactly one field `total: Num`, and
nothing more.

To express such constraints, Nickel features *row polymorphism*. The idea is
similar to polymorphism, but instead substituting a parameter for a single type,
we can substitute a parameter for a whole sequence of field declarations, also
referred to as rows:

```nickel
(let addTotal: forall a b. {total: Num | a} -> {total: Num | b} -> Num
  = fun r1 r2 => r1.total + r2.total in
let r1 = {jan = 200, feb = 300, march = 10, total = jan + feb} in
let r2 = {aug = 50, sept = 20, total = aug + sept} in
let r3 = {may = 1300, june = 400, total = may + june} in
{
  partial1 = addTotal r1 r2,
  partial2 = addTotal r2 r3,
}) : {partial1: Num, partial2: Num}
```

Result:
```
{partial1 = 570, partial2 = 1770}
```

In the type of `addTotal`, the part `{total: Num | a}` expresses exactly what we
wanted: the argument must have a field `total: Num`, but the *tail* (the rest of
the record type) is polymorphic, and `a` may be substituted for an arbitrary
field lists (like `jan: Num, feb: Num`). We used two different generic
parameters `a` and `b`, to express that the tails of the arguments may differ.
If we used `a` in both places, as in `forall a. {total: Num | a} -> {total: Num
| a} -> Num`, we could still write `addTotal {total = 1, foo = 1} {total = 2,
foo = 2}` but `addTotal {total = 1, foo = 1} {total = 2, bar = 2}` would be
rejected. Using distinct parameters `a` and `b` gives us maximum flexibility.

What comes before the tail may include several fields, is in e.g. `forall a.
{total: Num, subtotal: Num | a} -> Num`.

Note that row polymorphism also works with enums, with the same intuition of a
tail that can be substituted for something else. For example:

```nickel
let portOf : forall a. <http, ftp | a> -> Num = fun protocol =>
switch {
  `http -> 80,
  `ftp -> 21,
  _ -> 8000,
} protocol
```

### Take-away

The type system of Nickel has a few basic types (`Dyn`, `Num`, `Str`,
and `Bool`) and type constructors for lists, records, enums and functions.
Nickel features generics via polymorphism, introduced by the `forall` keyword. A
type can not only be generic in other types, but records and enums types can
also be generic in their tail. The tail is delimited by `|`.

## Interaction between statically typed and dynamically typed code

In the previous section, we've been focusing solely on the static typing side.
We'll now explore how typed and untyped code interact.

### Using statically typed code inside dynamically code

Until now, we have written the statically typed `filter` examples where
statically typed blocks included both the definition of `filter` and the call
sites. More realistically, `filter` would be a typed library function (it is
actually part of the standard library as `lists.filter`) while being likely to
be called from dynamically typed configuration file. In this situation, the call
site escape the typechecker. Thus, without an additional mechanism, static
typing would only ensure that the implementation of `filter` doesn't violate the
typing rules, but wouldn't prevent a ill-formed call from dynamically typed
code.  Thus, we haven't solved the main issue of delayed dynamic type errors
presented in the introduction! Remember, the typical problem is the caller
passing an value of the wrong type that eventually raises an error from within
`filter`.

Hopefully, Nickel has actually a good error reporting story in this situation.
Let us see by ourselves:

```nickel
lists.filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
```

Result:
```
error: Blame error: contract broken by the caller.
  ┌─ :1:17
  │
1 │ forall a. (a -> Bool) -> List a -> List a
  │                 ---- expected return type of a function provided by the caller
  │
  ┌─ repl-input-45:1:67
  │
1 │ lists.filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
  │                                                                   - evaluated to this expression
  │
  = This error may happen in the following situation:
    1. A function `f` is bound by a contract: e.g. `(Num -> Num) -> Num`.
    2. `f` takes another function `g` as an argument: e.g. `f = fun g => g 0`.
    3. `f` is called by with an argument `g` that does not respect the contract: e.g. `f (fun x => false)`.
  = Either change the contract accordingly, or call `f` with a function that returns a value of the right type.
  = Note: this is an illustrative example. The actual error may involve deeper nested functions calls.

note:
    ┌─ <stdlib/lists>:160:14
    │
160 │     filter : forall a. (a -> Bool) -> List a -> List a
    │              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ bound here

[...]
note:
  ┌─ repl-input-45:1:1
  │
1 │ lists.filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
  │ -------------------------------------------------------------------- (3) calling <func>
```

We call `filter` from a dynamically typed location, but still get a spot-on
error. To precisely avoid dynamically code injecting values of the wrong type
inside statically typed blocks via function calls, the interpreter protects said
blocks by a contract. Contracts form a principled runtime verification scheme .
Please refer to the [dedicated document](./contracts.md) for more details, but
for now, you can just remember that *any type annotation*, wherever it is, gives
rise at runtime to a corresponding contract application. In other words, `foo:
T` and `foo | T` (here `|` is contract application, not the row tail separator)
behave exactly the same at *runtime*.

Thanks to this protection, you can type your library functions and use them from
dynamically typed code while still getting good error messages.

### Using untyped code inside typed code

In the other direction, we face a different issue. How can we use untyped values
inside typed code? Let us try:

```
let x = 1 in
(1 + x : Num)
```

Although `x` shouldn't be statically typed, this code is accepted. Hmm. What
about:

```
let x = 0 + 1 in
(1 + x : Num)
```

Now, it doesn't, as we would initially expect. In all generality, you can use an
identifier defined in untyped code from within a typed block, as it is the case
here. Thus, the typechecker needs to assign a type to every identifier, even the
ones defined outside of statically typed block. But it also tries to respect the
intention of the programmer.  If one doesn't use annotations, then the code
shouldn't be typechecked, whatever the reason is (for performance, because it
can't be well-typed, etc.): if you want `x` to be typed, you should annotate it.

The typechecker still tries its best not to be too stupid. It is obvious in the
first case that `1` is of type `Num`. This information is cheap to gather. When
encountering a binding outside of a typed block, the typechecker determines the
*apparent type* of the definition. The rationale is that determining the
apparent type shouldn't recurse arbitrarily inside the expression or do anything
non-trivial.  Typically, our previous `0 + 1` is a compound expression with a
primitive operator, so the apparent type is just `Dyn`. For now, the typechecker
determines an apparent type that is not `Dyn` only for literals (numbers,
strings, booleans), variables, and annotated expressions.

Otherwise, the typechecker fallbacks to `Dyn`. In the future, it could also
infer `Dyn -> Dyn` for functions, `{_: Dyn}` for records, and so on. As of now
(INSERT VERSION HERE), it doesn't.

We could add a type annotation to `x`. But sometimes we don't want to, or we
can't. Maybe we know that the expression correctly evaluates to a number but the
typechecker rejects it because it isn't able to see it. In any of these
situations, we can trade a type annotation for a contract application:

Example:
```nickel
let x | Num = if true then 0 else "a" in
(1 + x : Num)
```

Here, `x` is clearly always a number, but it is not well-typed (the `then` and
`else` branches of an `if` must have the same type). Nonetheless, this program
is accepted! The rationale is that because we inserted a contract application,
the typechecker knows that if `x` is not a number, the program will fail early
with a detailed contract error. Thus, if we reach `1 + x`, `x` is necessarily a
number at this point and won't cause any type mismatch. In a way, the contract
application acts like a type cast, but whose verification is delayed to
run-time.

Dually to a static type annotation, a contract annotation *turns the typechecker
off again*. This is illustrated by the following variation being accepted:

```nickel
(1 + ((if true then 0 else "a" | Num)) : Num
```

While this one is rejected because of the mismatch between branches:

```nickel
(1 + (if true then 0 else "a")) : Num
```

Result:
```
error: Incompatible types
  ┌─ repl-input-46:1:27
  │
1 │ (1 + (if true then 0 else "a")) : Num
  │                           ^^^ this expression
  │
  = The type of the expression was expected to be `Num`
  = The type of the expression was inferred to be `Str`
  = These types are not compatible
```

### Take-away

When calling to typed code from untyped code, Nickel
automatically inserts contract checks at the boundary to enjoy clearer and
earlier error reporting. In the other direction, an expression `exp | Type` is
blindly accepted to be of type `Type` by the typechecker. This is a way of using
untyped values inside typed code by telling the typechecker "trust me on this
one, and if I'm wrong there will be a contract error anyway". Finally, while a
type annotation switches the typechecker on, a contract annotation switches it
back off.

## Typing in practice

When to use type annotation, a contract application, or none? This is what the
guide [Type versus contracts: when to?](./types-vs-contracts.md) is for!
