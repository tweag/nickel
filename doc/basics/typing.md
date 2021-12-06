# Typing in Nickel

## Preamble

Static typing or dynamic typing? Here comes the eternal debate of languages
aficionados. While the idea that static typing is pretty much a necessity for
standard large codebases, the case of an interpreted configuration language may
appear less clear-cut.

Nevertheless, whoever has ever faced puzzling dynamic type errors (as in
[Nix]()) may have felt the strong need for something better. Standard dynamic
typing is prone to error messages removed from the actual issue and a location
that doesn't point at the offending code . This is especially true when working
with functions, that tends to delay type errors by passing around ill-formed
values until it finally breaks when used in a totally different location.

On the other hand, for pure configuration code, static typing is less useful.
First, a configuration is a terminating program run once on fixed inputs: here,
basic type errors will show up at evaluation anyway. What's more, Nickel has a
powerful validation system, contracts, that can already do more.

For those reasons, Nickel takes an hybrid approach called *gradual typing*.
Gradual typing enables to mix both static typing and dynamic typing.

You should at least skim at this document first, but once you have even a
partial understanding of how typing works, you should look at the practical
guide [Type versus contracts: when to?](./types-vs-contracts.md) to help you
decide when and how to annotate your code.

## Typing modes

### Untyped by default

By default, Nickel code is assumed to be configuration code. It is thus run in
*untyped* mode (understand dynamically typed).

Ex:

While this is fine for configuration code, especially when checked against a
schema by a contract, it doesn't work so well once we are using functions.

Ex: flatten

This example shows how dynamic typing delays type errors, making them harder to
diagnose. Here, `flatten` is fine, but the error still points at its
implementation. The actual problem is that the caller provided an argument of
the wrong type: the last element should be a list, but it's a number. Alas, the
user (who is most probably not even the author of `flatten`) would have a hard
time reaching this conclusion from the error report in a similar but more
involved real situation.

### Typed blocks

The `flatten` example is the poster child for static typing. The typechecker
will catch the error earlier as the type expected by `flatten` and the type of
the argument don't match.

We use `:` to introduce a *type annotation*.  This annotation switches the
typechecker on for the annotated expression, be it a variable definition, a
record field or any expression using inline annotations. We will call such an
expression under a type annotation a *typed block*.

Examples:

Let us try on the flatten example. We want the call to be inside the typechecked
block. The easiest way is to add an annotation at the top-level:

Example

That's already much better! The error now points at the call site. What's more,
we just had to give the top-level annotation `List Num`. Nickel performs type
inference, so that you don't have to write the type of `flatten` or `[[1,2],
3]`. Although there's a twist about type inference and polymorphism (see
[dedicated section](#Polymorphism), but we'll come back to this later.

**Take-away**: Nickel has an hybrid approach to typing that mixes both static
typing and dynamic typing. The default is dynamic typing. The static typechecker
kicks in when you write a type annotation using `:`. This delimits a typed
block. Nickel has type inference, that spares you writing unnecessary type
annotations.

Now, let us have a quick tour of the type system.

## Type system
The basic types are:

- `Dyn`: the dynamic type. This is the type given to most expressions outside of 
  a typed block. A value of type `Dyn` can be pretty much anything.
- `Num`: the only number type. Currently implemented as a 64bits float.
- `Str`: strings.
- `Bool`: booleans, `true` or `false`.
<!-- - `Lbl`: a contract label. You usually don't need to use it or worry about it, -->
<!--     it is more of an internal thing.  -->

Nickel features the following type constructors:

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
  pair.fst : Num`
  ```
- **Dynamic record**: `{_: T}`. A record whose field
  names are statically unknown but are all of type `T`.  Typically used to model
  dictionaries. 

  Example:
  ```nickel
  let occurences : {_: Num} = {a = 1, b = 3, c = 0} in
  records.map (fun char count => count+1) : {_ : Num}
  ```
- **Enum**: `<tag1, .., tagn>`: an enumeration comprised of alternatives `tag1`,
  .., `tagn`. An enumeration literal is prefixed with a backtick and serialized
  as a string. It is useful to encode finite alternatives. The advantage over
  strings is that the typechecker handles them more finely: it is able to detect
  incomplete matches for example.

  Example: 
  ```nickel
  let protocol : <http, ftp, sftp> = `http in
  switch {
    http -> 1,
    ftp -> 2,
    sftp -> 3
  } protocol : Num
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

### Polymorphism (generics)

#### Plain polymorphism 

Let us try to write a type annotation for our `flatten` function:

```nickel
(let flatten = ... in
flatten [[1,2], [3,4]]) : List Num
```

What should it be? In this specific example, the type inferred by the
typechecker is `List (List Num) -> List Num`, guessed from the application to
the argument `[[1,2], [3,4]]`. Thus, if we try to use flatten on something else,
here is what we get:

```nickel
nickel>
(let flatten = ... in
let result = flatten [[1,2], [3,4]] in
let dummy = flatten [["a","b"], []] in
result) : List Num


```

That's too bad, because in practice the code of `flatten` is agnostic with
respect to the type of elements of the list. That is, `flatten` is *generic*.
This is expressed in Nickel with so-calleds *parametric polymorphism*. This is
really just a pedantic label for some flavour of generics. Generic parameters
are introduced by the keyword `forall` followed by an identifier. We can now fix
our example:

```nickel
(let flatten : forall a. List (List a) -> List a in
let result = flatten [[1,2], [3,4]] in
let dummy = flatten [["a","b"], []] in
result) : List Num
```

And now it works! `forall a. List (List a) -> List a` means it is ok to
substitute `a` for any other type. You can use as many generic parameters as you
wish:

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

**Remark**: if you are a more type-inclined reader out there, you may wonder why
the typechecker is not capable of inferring a polymorphic type for `flatten` by
itself. Indeed, Hindley-Milner type-inference can precisely infer heading
`foralls`, such that the first example without the polymorphic annotation would
already pass. We chose to abandon this so-called automatic generalization,
because it makes other aspects of the type system and type inference more
complex or limited. Requiring annotation of polymorphic functions seems like a
good practice and an acceptable price to pay in exchange of making the type
system simpler and more easily extensible, in a non type-heavy configuration
language.

#### Row polymorphism

In a configuration language, you will often find yourselves handling records of
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

The problem here is that the requirement of `addTotal` is that both arguments
have a field `total: Num`, but could very well have other fields, for all we
care. Unfortunately, we don't know right now how to express this constraint. The
type signature is too restrictive here, because it imposes that arguments have
exactly one field `total: Num`, and nothing more.

To solve the issue, Nickel has *row polymorphism*. The idea is similar to
polymorphism, but instead substituting a parameter for a single type, we can
substitute it for a sequence of fields. Let us first fix our working example:

```nickel
(let addTotal: forall a b. {total: Num | a} -> {total: Num | b} -> Num
  = fun r1 r2 => r1.total + r2.total in
let r1 = {jan = 200, feb = 300, march = 10, total = jan + feb} in
let r2 = {aug = 50, sept = 20, total = aug + sept} in
let r3 = {may = 1300, june = 400, total = may + june} in
{
  partial1 = addTotal r1 r2,
  partial2 = addTotal r2 r3,
}) : {patial1: Num, partial2: Num} 
```

In the function type, the part `{total: Num | a}` expresses exactly what we
wanted: the argument must have a field `total: Num`, but the *tail* (the rest of
the record type) is generic, and `a` may be substituted for an arbitrary field
lists (like `foo: Str, bar: Dyn`). We used two different generic parameters `a`
and `b`, to express that the tails of the arguments may differ. If we used `a`
in both places, as in `forall a. {total: Num | a} -> {total: Num | a} -> Num`, we
could still write `addTotal {total = 1, foo = 1} {total = 2, foo = 2}` but
`addTotal {total = 1, foo = 1} {total = 2, bar = 2}` would be rejected. Using
`a` and `b` gives us maximum flexibility.

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

**Take-away**: The type system of Nickel has a few basic types (`Num`, `Str`,
and `Bool`) and type constructors for lists, records, enums and functions.
Nickel features generics via polymorphism, introduced by the `forall` keyword. A
type can not only be generic in other types, but records and enums types can
also be generic in their tail. The tail is delimited by `|`.

In this section, we've been focusing solely on the static typing side. We'll now
see how typed and untyped code interact.

## Interaction between typed and untyped code

### Using typed code inside untyped code

I invite to you come back to our first love, the `flatten` example. In the
previous section, we included everything -- both the definition of `flatten` and
the call site -- in a typed block. More realistically, `flatten` would be a typed
library function (it is actually part of the stdlib as `lists.flatten`), but is
likely to be called from untyped configuration code. Without an
additional mechanism, this would just ensure that `flatten` is without flaws, but
since the call site escapes the typechecker, we may end up with the very same
initial problem. Remember, the caller passes an unchecked, invalid value that is raising
an error from within flatten.

Hopefully, Nickel prevents us from this happening. Let us see by ourselves:

```nickel
lists.flatten [[1,2], 3]
```

We call `flatten` from an untyped location, but still get a spot-on error.
What's is happening? The interpreter protects typed block by a
[contract](./contracts.md), which is a runtime check. Please refer to the
dedicated documentation for more details. For now, you can just remember that
*any type annotation*, wherever it is, gives rise at runtime to a corresponding
contract check. In other words, `foo: T` and `foo | T` (here `|` is contract
application, not the row tail separator) behave exactly the same at *runtime*.
This approach of protecting typed code from untyped code is known as *sound
gradual typing*.

### Using untyped code inside typed code

In the other direction, we face a different issue. How can we use untyped values
inside typed code? Let us try:

```
let x = 1 in
(1 + x : Num)
```

Although `x` should be untyped, it works! Hmm. What about:

```
let x = 0 + 1 in
(1 + x : Num)
```

Now, it doesn't, as we would expect originally. As it is the case here, you can use an
identifier defined in untyped code from within a typed block. Thus, the typechecker
needs to assign a type to every identifier, even outside of a typed block. But it
also tries to obey the will of the programmer. If one doesn't use annotations,
then the code shouldn't be typechecked, either for performance reasons,
because it can't be well-typed, etc. The reason doesn't matter: if you want
`x` to have type `Num`, you should annotate it.

The typechecker still tries its best to not be too stupid either. It is obvious
in the first case that `1` is of type `Num`. This information is cheap to
gather. Thus, when encountering a binding outside of a typed block, the
typechecker determines the *apparent type* of the definition. The rationale is
that determining the apparent type shouldn't recurse arbitrarily inside the
expression or do anything non-trivial.  Typically, the second case `0 + 1` is a
compound expression with a primitive operator, so the apparent type is just
`Dyn`. For now, it can infer literals, variables, and annotated expressions.
Otherwise, the typechecker fallbacks to `Dyn`. In the future, it could e.g.
assign `Dyn -> Dyn` to functions, or `{_: Dyn}` to records. As of now (INSERT
VERSION HERE), it doesn't.

We could add a type annotation to `x`. But sometimes we don't want to, or we
can't. Maybe we don't want the typechecker to do its ceremony and recurse in the
whole expression. Maybe the expression is not typeable in Nickel, but we still
know it will evaluate to a number. In any of these situations, we can trade the
type annotation for a contract application:

```
let x | Num = if true then 0 else "a" in
(1 + x : Num)
```

Here, `x` is clearly always a number, but it is not well-typed (the `then` and
`else` branches of an `if` must have the same type). Nonetheless, this program
is accepted! The rationale is that because we inserted a contract application,
the typechecker knows that if `x` is not a number, the program will fail early
with a detailed contract error. Thus, if we reach `1 + x`, `x` is necessarily a
number at this point, and everything will be fine. In a way, the contract
application acts like a type cast, but whose verification is delayed to
run-time.

When using a contract annotation inside typed code, the *typechecker is turned
off again*. Contract application is dual to type annotation with respect to
typechecking. The following variation is accepted:

```
(1 + ((if true then 0 else "a" | Num)) : Num
```

While, as said before, this one is rejected:

```
(1 + (if true then 0 else "a")) : Num
```

**Take-away**: When calling to typed code from untyped code, Nickel
automatically inserts contract checks at the boundary to enjoy clearer and
earlier error reporting. In the other direction, an expression `exp | Type` is
blindly accepted to be of type `Type` by the typechecker. This is a way of using
untyped values inside typed code by telling the typechecker "trust me on this
one, and if I'm wrong there will be a contract error anyway". Finally, while a
type annotation switches the typechecking mode to typed, a contract annotation
switches it back to untyped.

At this point, you may feel confused. We have static types on one side, we have
contracts on the other, but static type annotations actually behaves the same at
runtime, and contract annotations also have an effect on the typechecker. What's
more, they look like they cover similar use cases (check that some value has a
specific shape or behavior), and the annotations are written in the same type
syntax. How do we know when to use one and not the other? Take a look at [Type
versus contracts: when to?](./types-vs-contracts.md)!
