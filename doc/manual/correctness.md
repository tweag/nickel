---
slug: correctness
---

# Correctness in Nickel

One of the main value proposition of Nickel is to make configurations
programmable. However, a not less important one is to help you write *correct*
configurations. Our definition of correctness is not restricted to but includes
the following properties:

1. The evaluation doesn't end up on non-sensical expressions. For example, when
   trying to add a number to a string, or to call to a value which is not a
   function.
2. The generated configuration is valid. Your program may be correct with
   respect to the previous point while outputing a field `prto = "80"` instead
   of `port = 80`. The consumer of the configuration will probably fail.
   Typically, validity involves respecting a data schema.

Because Nickel an interpreted language, there is no well-defined *ahead-of-time*
phase that can prevent a class of errors once and for all[^1]. In practice, you
will often still get an error at the point you are trying to run a program.
However, what the language can do is to vastly improve the troubleshooting
experience by providing:

1. An ergonomic way to express properties to check code and data against.
2. Mechanisms to catch violation as early as possible (e.g. before the code
   path is triggered).
3. Informative error messages.

## Solutions

One standard device for correctness is static typing. The static typing
discipline brings in important benefits for large codebases of common
applications. But the case of an interpreted configuration language appears less
clear-cut.

For pure configuration code, which is mostly data, static typing is not as
useful. First, a configuration is a terminating program run once on fixed
inputs: here, basic type errors will show up at runtime anyway. Second, for data
validation, static types are too rigid. Statically checking that an expression
will always evaluate to a valid port number, for example, requires very advanced
machinery. On the other hand, checking this property at runtime on the final
result is trivial.

Nevertheless, if you have ever faced puzzling [dynamic type
errors](https://www.haskellforall.com/2021/01/dynamic-type-errors-lack-relevance.html),
you may feel the need for something better. Bare dynamic typing is prone to
irrelevant error messages, pointing to a location far from the problematic code
in the source. This is especially true when working with functions, which may be
called with incorrect values and pass them around until they eventually break
evaluation in a distant and seemingly unrelated location. For reusable code,
that is functions, static typing really helps.

## Types and contracts

This apparent dilemma is solved in Nickel by the combination of *gradual typing*
and *contracts*. Gradual typing is a typing discipline where you can use both
static typing and dynamic typing, at will. Contracts augment the dynamic typing
part by providing a principled and ergonomic way to enforce arbitrary assertions
at run-time.

In essence, the purpose of types and contracts is the same: to ensure that an
expression verifies some desired properties. And when it doesn't, the
interpreter must fail with an informative error message. There is a wide range
of properties that can be checked for, such as:

- To evaluate to a number.
- To evaluate to the same value as this other field `foo` of the same configuration.
- To evaluate to a record with at least one field `port` that is a valid port number.
- To be a function always mapping numbers to numbers.
- etc.

Both types and contracts are enforced in a similar way, using an annotation:

```
$ nickel repl
nickel> 1 + 1.5 : Num
2.5

nickel> let f : Num -> Num = fun x => x + 1
nickel> f 0
1

nickel> let GreaterThan = fun bound =>
  contract.from_predicate (fun val => val >= bound) in
-1 | GreaterThan 10

error: contract broken by value
[..]
```

`:` introduces a type annotation, while `|` introduces a contract annotation.
They support the same syntax for properties on the right-hand side.

**The fundamental difference between types and contracts is that type
annotations are checked statically, before the program even starts, while
contracts are checked lazily, at run-time**. The characteristics and use-cases of
types and contracts directly follow from this distinction.

In the next paragraphs, we consider two typical examples to illustrate the
difference between types and contracts in practice.

### Case 1: a function operating on arrays

Say we need a function to convert an array of key-value pairs to an array of keys
and an array of values. Let's call it `split`:

```
nickel> split [{key = "foo", value = 1}, {key = "bar", value = 2}]
{keys = ["foo", "bar"], values = [1, 2]}

nickel> split [
  {key = "firewall", value = true},
  {key = "grsec", value = false},
  {key = "iptables", value = true},
]
{ keys: ["firewall", "grsec", "iptables"], values [true, false, true] }
```

Here is the definition for `split`, but with a twist. We mistakenly forgot to
wrap `pair.key` as an array before concatenating at line 6:

```nickel
# lib.ncl
{
  split = fun pairs =>
    array.fold (fun pair acc =>
      {
        # problem: the right expression to use is [pair.key]
        keys = acc.keys @ pair.key,
        values = acc.values @ [pair.value],
      })
      {keys = [], values = []}
      pairs
}
```

And we call to split from our configuration:

```nickel
# config.ncl
let {split} = import "lib.ncl" in
split [{key = "foo", value = 1}, {key = "bar", value = 2}]
```

We want to ensure that the callers to `split` pass an array
verifying:
 - elements are records with a `key` field and a `value` field
 - keys are strings
 - values can be anything, but must all have the same type

We also want to make sure our implementation correctly returns a value which is
a record with a field `keys` that is an array of strings, and a field `values`
that is an array of elements of the same type as the input values.

An idiomatic way to express these properties in Nickel is to use the annotation

```nickel
forall a. Array {key: Str, value: a}
          -> {keys: Array Str, values: Array a}
```

The `forall` parts says that the type of values `a` can be anything, but it has
to be the same `a` in the input and in the output. We'll now see the difference
between enforcing this specification using a type annotation or a contract
annotation.

#### Using a contract annotation

A contract performs checks at run-time. At this stage, a function is mostly an
opaque, inert value, waiting for an argument to hand back a result. In
consequence, a function contract is doomed to fire only when `split` is applied
to an argument, in which case the contract checks that:

1. The argument satisfies the `Array {key: Str, value: a}` contract.
2. The return value satisfies the `{keys: Array Str, values: Array a}` contract.

Those checks produce useful error message when the caller passes arguments of
the wrong type, or the function returns a value of the wrong type. But the
function contract for `split` has the following limitations:

- It only checks the values arising in concrete calls. In particular, if `split`
  is not called by some program yet (e.g. written as part of a library), no check
  takes place at all. If a particular code path is not triggered, it won't be
  checked. **Here, evaluating or typechecking `lib.ncl` won't raise any error**.
- Only the input value and the return value are checked. If `split` mishandles
  an intermediate internal value not subject to another contract, we're
  potentially back into unhelpful dynamic type errors. Evaluating `config.ncl`
  indeed reports the following error:

        error: Type error
        ┌─ repl-input-12:6:27
        │
      6 │         keys = acc.keys @ pair.key,
        │                           ^^^^^^^^ This expression has type Str, but Array was expected
        │
        ┌─ repl-input-13:1:45
        │
      1 │ lib.split [{key = "foo", value = 1}, {key = "bar", value = 2}]
        │                                             ----- evaluated to this
        │
        = @, 2nd operand

  This error is not very helpful to the caller. It points to inside the
  implementation of `split`, and to the `key = "bar"` part of the argument,
  which respects the contract.

#### Using a type annotation

`split` is a generic function operating on builin types. This is a good
candidate for static typing, which will help:

- Ensure the property holds for all possible values of the parameter.
- Check all the expressions inside `split` as well.
- Report errors before execution takes place.

Even if the call site is not statically typed, a type annotation also gives rise
to a contract (see the [typing section][./typing.md] for more details). Thus,
any issue with the arguments passed or the return value will be caught and
correctly reported.

What's more, intermediate values are also typechecked. Evaluating or just
typechecking `lib.ncl` already reports an error:

```
error: Incompatible rows declaration
  ┌─ repl-input-14:9:7
  │
9 │       pairs
  │       ^^^^^ this expression
[..]
error: While typing field `key`: Incompatible types
 = The type of the expression was expected to be `Array Str`
 = The type of the expression was inferred to be `Str`
 = These types are not compatible
```

The errors says that the `key` field of the elements of `pairs` is a string, but
given the call to `fold`, it was expected to be lists of strings.

#### Function contracts

From this example, it seems typing is superior in pretty much every respect for
functions. Typechecking is indeed more exhaustive, and generally reports errors
earlier. In return, typechecking is rigid (it can reject valid programs) and
limited in the properties it can handle. Properties outside the range of being a
list, a record or a function with other builtin types inside, such as checking
that a function returning a positive number, are out of the scope of
typechecking (see case 2 below for another example). In such cases, where
typechecking is not an option, a function contract can be helpful.

### Case 2: a custom property

The previous example used builtin types: functions, records, lists, and strings.
To validate a configuration, you may want to check more elaborate properties.
You can do so by writing your own custom properties, referred to as custom
contract from now on:

```nickel
let OptLevel = contract.from_predicate (fun value =>
    array.elem value ["O0", "O1", "O2", "O3"]) in
{
  opt_level = "O2",
}
```

`OptLevel` checks that its argument is a valid optimization level, that is
either `"O0"`, `"O1"`, `"O2"` or `"O3"`. You can ignore the details of the
implementation of `OptLevel` for now. Writing custom contracts is more
extensively covered in the [contracts section](./contracts.md). As for the
previous example, let's see the difference between using a type annotation and a
contract annotation.

#### Using a type annotation

If we write:

```nickel
let level = 1 in
{
  opt_level : OptLevel = "0" ++ srings.from_num level,
}
```

We get:

```
error: incompatible types
  ┌─ repl-input-3:3:27
  │
3 │   opt_level : OptLevel = "0" ++ (if level == 1 then "1" else "2"),
  │                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ this expression
  │
  = The type of the expression was expected to be `OptLevel`
  = The type of the expression was inferred to be `Str`
  = These types are not compatible
```

Because `OptLevel` is a custom predicate, the typechecker can't prove that `"0"`
concatenated with `string.from_num 1"` is a valid optimization level. For that
matter, even `"01" : OptLevel` doesn't typecheck. It's possible to build values
that are accepted to be of type `OptLevel`, but it's very restricted and they
can't really be used in a meaningful way inside typed code, beside being passed
around.

#### Using a contract annotation

For such custom properties, a contract is the way to go:

```nickel
let level = 4 in
{
  opt_level | #OptLevel = "0" ++ string.from_num level,
}
```

This correctly reports an error, and even gives the computed offending value:

```
error: contract broken by a value.
  ┌─ :1:1
  │
1 │ OptLevel
  │ -------- expected type
  │
  ┌─ repl-input-4:3:27
  │
3 │   opt_level | OptLevel = "0" ++ string.from_num level,
  │                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^ applied to this expression
  │
  ┌─ <unknown> (generated by evaluation):1:1
  │
1 │ "04"
  │ ---- evaluated to this value
```

### Summary

Types and contracts are the two mechanisms to enforce correctness of Nickel
programs. Typechecking is a static and ahead-of-time mechanism, which is
exhaustive and reports errors early, but is rigid and limited in the properties
that can be checked. Contract checking is a run-time mechanism, making contracts
more flexible and able to enforce arbitrary user-defined properties, but may
reports less errors than typechecking, and report them later.

As a rule of thumb, you should usually use types for functions operating on
fairly generic data. Contracts are most adapted for data (records and lists),
especially data that ends up in the final configuration. To enforce custom
properties that are out of the scope of builtin types, you should use contracts,
either for functions or data.

You'll find a in-depth description of the type system and how to use it in the
[typing section](./typing.md). For contracts, refer to the
[contract section](./contracts.md). Finally, if you are looking for a quick
cheat-sheet about when to use static typing or contracts, go to
[Type versus contracts: when to?](./types-vs-contracts.md).

[^1]: This statement is to be nuanced. Using the Nickel LSP server in your \
  editor, for example, does perform checks ahead of time, while you are typing \
  program. You can also perform typechecking separately before distributing a \
  configuration using `nickel typecheck`. However, a source program is not \
  guaranteed to have been checked in any way before execution.
