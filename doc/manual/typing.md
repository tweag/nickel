---
slug: typing
---

# Typing in Nickel

(For the motivations behind typing and a high-level overview of contracts and
types, first read the [correctness](./correctness.md) section.)

## Typing modes

### Dynamic typing

By default, Nickel code is dynamically typed. For example:

```nickel
{
  name = "hello",
  version = "0.1.1",
  fullname =
    if builtin.is_number version then
      "hello-v%{string.from_number version}"
    else
      "hello-%{version}",
}
```

As long as we operate on basic data (numbers, strings, etc.), dynamic type error
can be sufficient. Let us introduce an error on the last line of the previous
example:

```nickel
{
  name = "hello",
  version = "0.1.1",
  fullname =
    if builtin.is_number version then
      "hello-v%{string.from_number version}"
    else
      "hello-%{version + 1}",
}
```

`version` is a string, and can't be added to a number. If we try to export this
configuration using `nickel export`, we get a reasonable error message:

```text
error: Type error
  ┌─ repl-input-3:8:16
  │
3 │   version = "0.1.1",
  │             ------- evaluated to this
  ·
8 │       "hello-%{version + 1}",
  │                ^^^^^^^ This expression has type String, but Number was expected
  │
  = +, 1st argument

```

While dynamic typing is fine for configuration code, the trouble begins once we
are using functions. Say we want to filter over an array of elements:

```nickel
let filter = fun pred l =>
  array.fold_left (fun acc x => if pred x then acc @ [x] else acc) [] l in
filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
```

Result:

```text
error: Type error
  ┌─ repl-input-11:2:32
  │
2 │   array.fold_left (fun acc x => if pred x then acc @ [x] else acc) [] l in
  │                                ^^^^^^ This expression has type Number, but Bool was expected
3 │ filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
  │                                                             - evaluated to this
  │
  = if
```

This example illustrates how dynamic typing delays type errors, making them
harder to diagnose. Here, `filter` is fine, but the error still points to inside
its implementation. The actual issue is that the caller provided an argument of
the wrong type: the filtering function should return a boolean but returns
either the original element (a number) or `null`. This is a tiny example, so
debugging is still doable here. In a real code base, the user (who probably
wouldn't even be the author of `filter`) might have a harder time solving the
issue from the error report.

### Static typing

The `filter` example is the poster child for static typing. The typechecker will
catch the error early as the type expected by `filter` and the return type of
the filtering function passed as the argument don't match .

To call the typechecker to the rescue, use `:` to introduce a *type annotation*.
This annotation switches the typechecker on inside the annotated expression, be
it a variable definition, a record field or any expression using an inline
annotation. We will refer to such an annotated expression as a *statically typed
block*.

Example:

```nickel
# Let binding
let f : Number -> Bool = fun x => x % 2 == 0 in

# Record field
let r = {
  count : Number = 2354.45 * 4 + 100,
} in

# Inline
1 + ((if f 10 then 1 else 0) : Number)
```

Let us try on the filter example. We want the call to be inside the statically
typechecked block. The easiest way is to capture the whole expression by adding
a type annotation at the top-level:

```nickel
(let filter = fun pred l =>
     array.fold_left (fun acc x => if pred x then acc @ [x] else acc) [] l in
filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]) : Array Number
```

Result:

```text
error: Incompatible types
  ┌─ repl-input-12:3:37
  │
3 │ filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]) : Array Number
  │                                     ^ this expression
  │
  = The type of the expression was expected to be `Bool`
  = The type of the expression was inferred to be `Number`
  = These types are not compatible
```

This is already better! The error now points at the call site, and inside our
anonymous function, telling us it is expected to return a boolean instead of a
number. Notice how we just had to give the top-level annotation `Array Number`.
Nickel performs type inference, so that you don't have to write the type for
`filter`, the filtering function nor the array.

### Take-away

Nickel is gradually typed, meaning you can mix both static typing and dynamic
typing. The default is dynamic typing. The static typechecker kicks in when
using a type annotation `exp : Type`, which delimits a statically typed block.

Nickel also has type inference, sparing you writing unnecessary type
annotations.

## Type system

Let us now have a quick tour of the type system. The basic types are:

- `Dyn`: the dynamic type. This is the type given to most expressions outside of
  a typed block. A value of type `Dyn` can be pretty much anything.
- `Number`: the only number type. Currently implemented as a 64bits float.
- `String`: a string, which must always be valid UTF8.
- `Bool`: a boolean, that is either `true` or `false`.
<!-- - `Lbl`: a contract label. You usually don't need to use it or worry about it,-->
<!--     it is more of an internal thing.  -->

The following type constructors are available:

- **Array**: `Array T`. An array of elements of type `T`.

  Example:

  ```nickel
  let x : Array (Array Number) = [[1,2], [3,4]] in
  array.flatten x : Array Number
  ```

- **Record**: `{field1: T1, .., fieldn: Tn}`. A record whose field
  names are known statically as `field1`, .., `fieldn`, respectively of type
  `T1`, .., `Tn`.

  Example:

  ```nickel
  let pair : {fst: Number, snd: String} = {fst = 1, snd = "a"} in
  pair.fst : Number
  ```

- **Dictionary**: `{_: T}`. A record whose field
  names are statically unknown but are all of type `T`. Typically used to model
  dictionaries.

  Example:

  ```nickel
  let occurrences : {_: Number} = {a = 1, b = 3, c = 0} in
  record.map (fun char count => count + 1) occurrences : {_ : Number}
  ```

- **Enum**: ``[| `tag1, .., `tagn |]``: an enumeration comprised of alternatives
  `tag1`, .., `tagn`. An enumeration literal is prefixed with a backtick and
  serialized as a string. It is useful to encode finite alternatives. The
  advantage over strings is that the typechecker handles them more finely: it is
  able to detect incomplete matches, for example.

  Example:

  ```nickel
  let protocol : [| `http, `ftp, `sftp |] = `http in
  (protocol |> match {
    `http => 1,
    `ftp => 2,
    `sftp => 3
  }) : Number
  ```

- **Arrow (function)**: `S -> T`. A function taking arguments of type `S` and
  returning a value of type `T`. For multi-parameters functions, just iterate
  the arrow constructor.

  Example:

  ```nickel
  {
    incr : Number -> Number = fun x => x + 1,
    mkPath : String -> String -> String -> String = fun basepath filename ext =>
      "%{basepath}/%{filename}.%{ext}",
  }
  ```

### Polymorphism

#### Type polymorphism

Usually, a function like `filter` would be defined in a library. In this case,
it is good practice to write a type annotation for it, if only to provide the
consumers of this library with an explicit interface. What should be the type
annotation for `filter`?

In our initial `filter` example, we are filtering on an array of numbers. But the
code of `filter` is agnostic with respect to the type of elements of the array.
That is, `filter` is *generic*. Genericity is expressed in Nickel through
*polymorphism*. A polymorphic type is a type that contains the keyword `forall`,
which introduces type variables that can later be substituted for any concrete
type. Here is our polymorphic type annotation for `filter`:

```nickel
{
  filter : forall a. (a -> Bool) -> Array a -> Array a = ...,
}
```

Now, filter can be used on numbers as in our initial example, but on strings as
well:

```nickel
{
  foo : Array String = filter (fun s => string.length s > 2) ["a","ab","abcd"],
  bar : Array Number = filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6],
}
```

You can use as many parameters as you need:

```nickel
let fst : forall a b. a -> b -> a = fun x y => x in
let snd : forall a b. a -> b -> b = fun x y => y in
{ n = fst 1 "a", s = snd 1 "a" } : {n: Number, s: String}
```

Or even nest them:

```nickel
let higherRankId : forall a. (forall b. b -> b) -> a -> a
  = fun id x => id x in
let id : forall a. a -> a
  = fun x => x in
higherRankId id 0 : Number
```

#### Type inference and polymorphism

If we go back to our first example of the statically typed `filter` without the
polymorphic annotation and try to add a call to `filter` on an array of strings,
the typechecker surprisingly rejects our code:

```nickel
(let filter = ... in
let result = filter (fun x => x % 2 == 0) [1,2,3,4,5,6] in
let dummy = filter (fun s => string.length s > 2) ["a","ab","abcd"] in
result) : Array Number
```

Result:

```text
error: Incompatible types
  ┌─ repl-input-35:2:37
  │
2 │ let dummy = filter (fun s => string.length s > 2) ["a","ab","abcd"] in
  │                                            ^ this expression
  │
  = The type of the expression was expected to be `String`
  = The type of the expression was inferred to be `Number`
  = These types are not compatible
```

The reason is that **without an explicit polymorphic annotation, the typechecker
will always infer non-polymorphic types**. If you need polymorphism, you have to
write a type annotation. Here, `filter` is inferred to be of type `(Number -> Bool)
-> Array Number -> Array Number`, guessed from the application in the right hand side of
`result`.

**Note**:
if you are a more type-inclined reader, you may wonder why the typechecker is
not capable of inferring a polymorphic type for `filter` by itself. Indeed,
[Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
type-inference can precisely infer heading `foralls`, such that the previous
rejected example would be accepted. We chose to abandon this so-called automatic
generalization, because doing so just makes things simpler with respect to the
implementation, the design and the extensibility of the language and the type
system. Requiring annotation of polymorphic functions seems like a good practice
and a small price to pay in return, in a non type-heavy configuration language
like Nickel.

#### Row polymorphism

In a configuration language, you will often find yourself handling records of
various kinds. In a simple type system, you can hit the following issue:

```nickel
(let addTotal: {total: Number} -> {total: Number} -> Number
  = fun r1 r2 => r1.total + r2.total in
let r1 = {jan = 200, feb = 300, march = 10, total = jan + feb} in
let r2 = {aug = 50, sept = 20, total = aug + sept} in
let r3 = {may = 1300, june = 400, total = may + june} in
{
  partial1 = addTotal r1 r2,
  partial2 = addTotal r2 r3,
}) : {partial1: Number, partial2: Number}
```

```text
error: Type error: extra row `sept`
  ┌─ repl-input-40:8:23
  │
8 │   partial2 = addTotal r2 r3,
  │                       ^^ this expression
  │
  = The type of the expression was expected to be `{total: Number}`, which does not contain the field `sept`
  = The type of the expression was inferred to be `{total: Number, sept: Number, aug: Number}`, which contains the extra field `sept`
```

The problem here is that for this code to run fine, the requirement of
`addTotal` should be that both arguments have a field `total: Number`, but could
very well have other fields, for all we care. Unfortunately, we don't know right
now how to express this constraint. The current annotation is too restrictive,
because it imposes that arguments have exactly one field `total: Number`, and
nothing more.

To express such constraints, Nickel features *row polymorphism*. The idea is
similar to polymorphism, but instead of substituting a parameter for a single type,
we can substitute a parameter for a whole sequence of field declarations, also
referred to as rows:

```nickel
(let addTotal: forall a b. {total: Number ; a} -> {total: Number ; b} -> Number
  = fun r1 r2 => r1.total + r2.total in
let r1 = {jan = 200, feb = 300, march = 10, total = jan + feb} in
let r2 = {aug = 50, sept = 20, total = aug + sept} in
let r3 = {may = 1300, june = 400, total = may + june} in
{
  partial1 = addTotal r1 r2,
  partial2 = addTotal r2 r3,
}) : {partial1: Number, partial2: Number}
```

Result:

```nickel
{partial1 = 570, partial2 = 1770}
```

In the type of `addTotal`, the part `{total: Number ; a}` expresses exactly what we
wanted: the argument must have a field `total: Number`, but the *tail* (the rest of
the record type) is polymorphic, and `a` may be substituted for arbitrary fields
(such as `jan: Number, feb: Number`). We used two different generic parameters `a` and
`b`, to express that the tails of the arguments may differ.  If we used `a` in
both places, as in `forall a. {total: Number ; a} -> {total: Number ; a} -> Number`, we
could still write `addTotal {total = 1, foo = 1} {total = 2, foo = 2}` but not
`addTotal {total = 1, foo = 1} {total = 2, bar = 2}`. Using distinct parameters
`a` and `b` gives us maximum flexibility.

What comes before the tail may include several fields, is in e.g. `forall a.
{total: Number, subtotal: Number ; a} -> Number`.

Note that row polymorphism also works with enums, with the same intuition of a
tail that can be substituted for something else. For example:

```nickel
let port_of : forall a. [| `http, `ftp; a |] -> Number =
  match {
    `http => 80,
    `ftp => 21,
    _ => 8000,
  }
```

Because the `match` statement has a catch-all case `_`, this function is indeed
able to handle other tags than `http` and `ftp`, as expressed by its polymorphic
type.

### Take-away

The type system of Nickel has usual basic types (`Dyn`, `Number`, `String`, and
`Bool`) and type constructors for arrays, records and functions. Nickel
features generics via polymorphism, introduced by the `forall` keyword. A type
can not only be generic in other types, but records types can also be
generic in their tail. The tail is delimited by `|`.

## Interaction between statically typed and dynamically typed code

In the previous section, we've been focusing solely on the static typing side.
We'll now explore how typed and untyped code interact.

### Using statically typed code inside dynamically code

Until now, we have written the statically typed `filter` examples using
statically typed blocks that enclosed both the definition of `filter` and the
call sites. More realistically, `filter` would be a statically typed library
function (it is actually part of the standard library as `array.filter`) and
likely be called from dynamically typed configuration files. In this situation,
the call site escapes the typechecker. Thus, without an additional mechanism,
static typing would only ensure that the implementation of `filter` doesn't
violate the typing rules, but wouldn't prevent an ill-formed call from
dynamically typed code.  At first sight, static typing hasn't solved the
original issue of delayed dynamic type errors at all! Remember, the typical
problem is the caller passing a value of the wrong type that eventually raises
an error from within `filter`.

Fortunately, Nickel does have a mechanism to prevent this from happening and to
provide good error reporting in this situation. Let us see that by ourselves by
calling to the statically typed `array.filter` from dynamically typed code:

```nickel
array.filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
```

Result:

```text
error: Blame error: contract broken by the caller.
  ┌─ :1:17
  │
1 │ forall a. (a -> Bool) -> Array a -> Array a
  │                 ---- expected return type of a function provided by the caller
  │
  ┌─ repl-input-45:1:67
  │
1 │ array.filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
  │                                                                   - evaluated to this expression
  │
  = This error may happen in the following situation:
    1. A function `f` is bound by a contract: e.g. `(Number -> Number) -> Number`.
    2. `f` takes another function `g` as an argument: e.g. `f = fun g => g 0`.
    3. `f` is called by with an argument `g` that does not respect the contract: e.g. `f (fun x => false)`.
  = Either change the contract accordingly, or call `f` with a function that returns a value of the right type.
  = Note: this is an illustrative example. The actual error may involve deeper nested functions calls.

note:
    ┌─ <stdlib/array>:160:14
    │
160 │     filter : forall a. (a -> Bool) -> Array a -> Array a
    │              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ bound here

[...]
note:
  ┌─ repl-input-45:1:1
  │
1 │ array.filter (fun x => if x % 2 == 0 then x else null) [1,2,3,4,5,6]
  │ ------------------------------------------------------------------- (3) calling <func>
```

We call `filter` from a dynamically typed location, but still get a spot-on
error. To precisely avoid dynamically code injecting values of the wrong type
inside statically typed blocks via function calls, the interpreter protects said
blocks by a contract. Contracts form a principled runtime verification scheme.
Please refer to the [dedicated manual section](./contracts.md) for more details,
but for now, you can just remember that *any type annotation* (wherever it is)
gives rise at runtime to a corresponding contract application. In other words,
`foo: T` and `foo | T` (here `|` is contract application, not the row tail
separator) behave exactly the same at *runtime*.

Thanks to this guard, you can statically type your library functions and use
them from dynamically typed code while still enjoying good error messages.

### Using dynamically typed code inside statically typed code

In the other direction, we face a different issue. Because dynamically typed
code just get assigned the `Dyn` type most of the time, we can't use a
dynamically typed value inside a statically typed block directly:

```nickel
let x = 0 + 1 in
(1 + x : Number)
```

Result:

```text
error: Incompatible types
  ┌─ repl-input-6:1:6
  │
1 │ (1 + x : Number)
  │      ^ this expression
  │
  = The type of the expression was expected to be `Number`
  = The type of the expression was inferred to be `Dyn`
  = These types are not compatible
```

We could add a type annotation to `x`. But sometimes we don't want to, or we
can't. Maybe in a real use-case, `x` is an expression that we know correctly
evaluates to a number but is rejected by the typechecker because it uses dynamic
idioms. In this case, we can trade a type annotation for a contract application:

Example:

```nickel
let x | Number = if true then 0 else "a" in
(1 + x : Number)
```

Here, `x` is clearly always a number, but it is not well-typed (the `then` and
`else` branches of an `if` must have the same type). Nonetheless, this program
is accepted! Because we inserted a contract application, the typechecker can be
sure that if `x` is not a number, the program will fail early with a detailed
contract error. Thus, if we reach `1 + x`, at this point `x` is necessarily a
number and won't cause any type mismatch. In a way, the contract application
acts like a type cast, but whose verification is delayed to run-time.

Dually to a static type annotation, a contract application also *turns the
typechecker off again*. You are back in the dynamic world. Even in a statically
typed block, a contract application can thus serve to embed dynamically typed
code that you know is correct but wouldn't typecheck:

```nickel
(1 + ((if true then 0 else "a" | Number)) : Number
```

The code above is accepted, while a fully statically typed version is rejected
because of the type mismatch between the if branches:

```nickel
(1 + (if true then 0 else "a")) : Number
```

Result:

```text
error: Incompatible types
  ┌─ repl-input-46:1:27
  │
1 │ (1 + (if true then 0 else "a")) : Number
  │                           ^^^ this expression
  │
  = The type of the expression was expected to be `Number`
  = The type of the expression was inferred to be `String`
  = These types are not compatible
```

**Apparent type**: As a side note, annotations are not always needed to use
dynamically typed code inside a statically typed block. The following example is
accepted:

```nickel
let x = 1 in
(1 + x : Number)
```

The typechecker tries to respect the intent of the programmer. If one doesn't
use annotations, then the code shouldn't be typechecked, whatever the reason is.
If you want `x` to be statically typed, you should annotate it.

That being said, the typechecker still avoids being too rigid: it is obvious in
the previous example case that `1` is of type `Number`. This information is cheap
to gather. When encountering a binding outside of a typed block, the typechecker
determines the *apparent type* of the definition. The rationale is that
determining the apparent type shouldn't recurse arbitrarily inside the
expression or do anything non-trivial. Typically, replacing `1` with a compound
expression `0 + 1` changes the type of `x` type to `Dyn` and makes the example
fail. For now, the typechecker determines an apparent type that is not `Dyn`
only for literals (numbers, strings, booleans), arrays, variables, imports and
annotated expressions. Otherwise, the typechecker falls back to `Dyn`. It may
do more in the future (assign `Dyn -> Dyn` to functions, `{_: Dyn}` to records,
etc).

### Take-away

When calling to typed code from untyped code, Nickel automatically inserts
contract checks at the boundary to enjoy clearer and earlier error reporting. In
the other direction, an expression `exp | Type` is blindly accepted to be of
type `Type` by the typechecker. This is a way of using untyped values inside
typed code by telling the typechecker "trust me on this one, and if I'm wrong
there will be a contract error anyway". While a type annotation switches the
typechecker on, a contract annotation switches it back off.

## Using contracts as types

**Warning**: as of version 0.2.x, using contracts as types won't work in the
majority of cases (typechecking will fail with a "can't compare contract types"
error). This is temporary and will likely be fixed in the upcoming 0.3.x
release. For more details, see issues
[#701](https://github.com/tweag/nickel/issues/701) and
[#724](https://github.com/tweag/nickel/issues/724).

<!-- TODO: find a good name for this section. Will need rework after merging
types and contracts syntax -->

Type annotations and contracts share the same syntax. This means that you can
technically use custom contracts as any other type inside a static type
annotation.

```nickel
let Port = contract.from_predicate (fun value =>
  builtin.is_number value
  && value % 1 == 0
  && value >= 0
  && value <= 65535) in

(10 - 1 : Port)
```

But this program is unfortunately rejected by the typechecker:

Result:

```text
error: Incompatible types
  ┌─ repl-input-0:7:2
  │
7 │ (10 : Port)
  │  ^^ this expression
  │
  = The type of the expression was expected to be `Port`
  = The type of the expression was inferred to be `Number`
  = These types are not compatible
```

It turns out statically ensuring that an arbitrary expression will eventually
respects an arbitrary user-written predicate is a really hard problem even in
simple cases (technically, it is even undecidable in the general case). The
typechecker doesn't have a clue about the relation between numbers and ports.
So, what can it do with annotations like `Port`? There is one situation when
the typechecker can be sure that something will eventually be a port number, or
will fail with the correct error message: when using a contract application.

```nickel
(let p | Port = 10 - 1 in
 let id = fun x => x in
 id p
) : Port
```

A custom contract hence acts like an opaque type (sometimes called abstract type
as well) for the typechecker. The typechecker doesn't really know much about it
except that the only way to construct a value of type `Port` is to use contract
application. You also need an explicit contract application to cast back a
`Port` to a `Number`: `(p | Number) + 1 : Number`.

Because of the rigidity of opaque types, using custom contracts inside static
type annotations is not very useful right now. We just had to give them a
reasonable meaning at typechecking time because types and contracts share the
same specification syntax, and they can thus appear inside types.

## Typing in practice

When to use type annotation, a contract application, or none of those? This is
what the guide [Type versus contracts: when to?](./types-vs-contracts.md) is
for.
