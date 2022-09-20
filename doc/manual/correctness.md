---
slug: correctness
---

# Correctness in Nickel

One of the main value propositions of Nickel is to make configurations
programmable. However, an equally important aim is to help developers write
*correct* configurations. Our definition of correctness includes, but is not
restricted to, the following properties:

1. Evaluation of nonsensical expressions is not permitted. For example, trying
   to add a number to a string, or calling a value which is not a function.
2. Generated configurations are valid. For example, a program may be correct
   with respect to the previous point while mistakenly outputting a field
   `prto = "80"` instead of `port = 80`. This means consumers of this
   configuration are likely to fail. Typically, validity involves respecting
   some data schema.

Because Nickel is an interpreted language, there is no well-defined
*ahead-of-time* phase during which errors can be detected once and for all[^1].
In practice, errors will still be raised when trying to run the program.
However, what Nickel *can* do is vastly improve the troubleshooting experience
by providing:

1. an ergonomic way to express properties of code and data which should be
   checked,
2. mechanisms to catch violations as early as possible (i.e., before a code path
   is triggered),
3. informative error messages.

## Solutions

One standard device for correctness is static typing, which can have obvious
benefits for many kinds of application. However, the case of an interpreted
configuration language is somewhat less clear-cut.

For pure configuration code, consisting predominantly of data, static typing is
perhaps less useful than in other kinds of application. Firstly, a configuration
is a terminating program run once on fixed inputs, so basic type errors will
show up at runtime. Secondly, for data validation, static types are too rigid.
For example, statically checking that an expression will always evaluate to a
valid port number requires very advanced machinery. On the other hand, checking
this property at runtime is trivial.

Nevertheless, those who have faced puzzling [dynamic type errors], may desire
something better. Bare dynamic typing is prone to irrelevant error messages,
pointing to locations far from the problematic code. This is especially true
when working with functions, which may be called with incorrect values which
eventually break evaluation in a distant and seemingly unrelated place. In these
cases, static typing can provide huge benefits.

[dynamic type errors]: https://haskellforall.com/2021/01/dynamic-type-errors-lack-relevance.html

## Types and contracts

This apparent dilemma is solved in Nickel through a combination of *gradual
typing* and *contracts*. Gradual typing is a typing discipline where both static
and dynamic typing may be used at will. Contracts augment dynamic typing by
giving developers a principled and ergonomic way to enforce arbitrary assertions
at runtime.

In essence, the purpose of types and contracts is the same: to ensure that an
expression verifies some desired properties and, when it does not, to fail with
an informative error message.

A wide range of properties can be checked for, such as:

- that an expression evaluates to a number,
- that some field `foo` evaluates to the same value as some other field `bar` of
  the same configuration,
- that an expression evaluates to a record with at least one field, `port`,
  whose value is a valid port number,
- that a given expression is a function mapping numbers to numbers
- etc.

Types and contracts are enforced in a similar way, via annotations.

Type annotations are introduced with `:`. For example:

```text
$ nickel repl

nickel> 1 + 1.5 : Num
2.5

nickel> let f : Num -> Num = fun x => x + 1
nickel> f 0
1

nickel> "not a Num" : Num
error: incompatible types
[..]
```

Contract annotations are introduced with `|`. For example:

```text
$ nickel repl

nickel> let GreaterThan = fun bound =>
  contract.from_predicate (fun val => val >= bound) in
-1 | GreaterThan 10

error: contract broken by value
[..]
```

Both type and contract annotations support the same syntax for properties on
their right-hand side.

**The fundamental difference between types and contracts is that type
annotations are checked statically, before the program even starts, whereas
contract annotations are checked lazily, at run-time**. The characteristics and
use-cases of types and contracts directly follow from this distinction.

In the following sections, we consider two typical examples to illustrate the
practical differences between types and contracts.

### Case 1: a function operating on arrays

Suppose we need a function to convert an array of key-value pairs into an array
of keys and an array of values. Let's call it `split`:

```text
nickel> split [{key = "foo", value = 1}, {key = "bar", value = 2}]
{keys = ["foo", "bar"], values = [1, 2]}

nickel> split [
  {key = "firewall", value = true},
  {key = "grsec", value = false},
  {key = "iptables", value = true},
]
{ keys: ["firewall", "grsec", "iptables"], values [true, false, true] }
```

Here is the definition for `split`, but with a twist. On line 6 we accidentally
try to pass the string `pair.key` to the concatenation operation `@`, without
first wrapping it in an array:

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

We call `split` from our configuration file:

```nickel
# config.ncl
let {split} = import "lib.ncl" in
split [{key = "foo", value = 1}, {key = "bar", value = 2}]
```

We wish to ensure that the callers to `split` pass an array with the following
properties:

- elements are records with `key` and `value` fields,
- `key`s are strings,
- `value`s can be anything, but all must have the same type.

We also want to make sure our implementation returns a record with fields `keys`
and `values`, where `keys` is an array of strings, and `values` is an array of
elements of the same type as the input `value`s.

An idiomatic way to express these properties in Nickel is to use the following
annotation:

```nickel
forall a. Array {key: Str, value: a}
          -> {keys: Array Str, values: Array a}
```

Where `forall a.` means that `a` can be any type, but that the `a` in the input
type must match the `a` in the output type.

We'll now consider the differences that arise when enforcing this specification
using contract and type annotations.

#### Using a contract annotation

`split` can be given a contract annotation as follows:

```nickel
split | forall a. Array {key: Str, value: a} -> {keys: Array Str, values: Array a} = # etc.
```

Contract annotations are checked at runtime. At this point functions are
essentially opaque values which must be passed an argument in order to evaluate
further. As a result, `split`'s contract will only be checked when the function
is actually applied to an argument. When this happens, the contract checks that:

1. the provided argument satisfies the `Array {key: Str, value: a}` contract,
2. the return value satisfies the `{keys: Array Str, values: Array a}` contract.

Those checks produce useful error message when the caller passes arguments of
the wrong type, or if function were to return a value of the wrong type. But the
function contract for `split` has the following limitations:

- It only checks values arising from concrete calls. In particular, if `split`
  is not called - e.g. when part of a library - no checks take place. This can
  be tested by evaluating or typechecking `lib.ncl` and observing that no errors
  are raised.
- Only the input value and the return value are checked against the contract. If
  `split` mishandles an intermediate value - as indeed it currently does - then
  the caller is left with only unhelpful dyamic type errors. For example,
  evaluating `config.ncl` reports the following error:

  ```text
  error: type error
    ┌─ /path/to/lib.ncl:6:27
    │
  6 │         keys = acc.keys @ pair.key,
    │                           ^^^^^^^^ this expression has type Str, but Array was expected
    │
    ┌─ /path/to/config.ncl:2:41
    │
  2 │ split [{key = "foo", value = 1}, {key = "bar", value = 2}]
    │                                         ----- evaluated to this
    │
    = @, 2nd operand

  ```

  From the caller's perspective, this is not a particularly helpful error. For
  one thing, it points inside the body of `split`, which the caller may not
  easily be able to change. At the same time, it points to the `key = "bar"`
  part of the argument, which is not problematic and indeed respects the
  contract.

#### Using a type annotation

`split` is a generic function operating on builtin types, which makes it a good
candidate for static typing. The presence of static types here will help ensure
that:

1. the property expressed in the annotation above holds for all possible values
   of the type parameter `a`,
2. all expressions in the body of `split` also typecheck,
3. errors are reported before any code is executed.

`split` can be given a type annotation as follows:

```nickel
split : forall a. Array {key: Str, value: a} -> {keys: Array Str, values: Array a} = # etc.
```

Type annotations also give rise to contracts, which means that even if `split`'s
callsite is not statically typed, problems with arguments or the return value
will still be caught and reported correctly. (See the [documentation on static
typing](./typing.md) for more details).

Intermediate values are also typechecked. Evaluating or typechecking `lib.ncl`
now reports an error:

```text
error: incompatible rows declaration
   ┌─ /path/to/lib.ncl:10:7
   │
10 │       pairs
   │       ^^^^^ this expression
   │
[..]
error: While typing field `key`: incompatible types
 = The type of the expression was expected to be `Array Str`
 = The type of the expression was inferred to be `Str`
 = These types are not compatible

```

The error says that the `key` field of the elements of `pairs` is a string, but
because of the call to `fold`, it was expected to be lists of strings.

#### Function contracts

From the example above, it may seem that when it comes to functions,
typechecking is superior to contract validation in every respect. Typechecking
is more exhaustive and errors are reported earlier. In return, however,
typechecking can cause valid programs to be rejected, and the properties which
can be checked are limited. For example, checking that a function always returns
a positive number is outside the scope of typechecking.

In such cases, function contracts can help fill the gap.

### Case 2: checking a custom property

The previous example used only builtin types: functions, records, lists, and
strings. It is likely that in the course of validating a configuration, you
may wish to check more elaborate properties. This can be done by writing a
custom contract.

For example, `OptLevel` is a contract which checks that a provided optimization
level is valid:

```nickel
# lib.ncl
{
  OptLevel = contract.from_predicate (fun value =>
    array.elem value ["O0", "O1", "O2", "O3"])
}
```

The details of the implementation can be ignored for now. It is enough to know
that a value passes the check if it is one of `"O0"`, `"O1"`, `"O2"` or `"O3"`.
Writing custom contracts is covered in more detail in [the relevant documentation](./contracts.md).

As in the previous example, we will consider the differences arising when using
`OptLevel` as a type and contract annotation.

#### Using a type annotation

If we write:

```nickel
# config.ncl
let {OptLevel} = import "lib.ncl" in
let level = 1 in
{
  opt_level : OptLevel = "A" ++ string.from_num level,
}
```

We get:

```text
error: incompatible types
  ┌─ /path/to/config.ncl:4:26
  │
4 │   opt_level : OptLevel = "A" ++ string.from_num level,
  │                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ this expression
  │
  = The type of the expression was expected to be `OptLevel`
  = The type of the expression was inferred to be `Str`
  = These types are not compatible
```

Because `OptLevel` is a custom predicate, the typechecker is unable to check
whether `"A"` concatenated with `string.from_num 1"` is a valid value. For that
matter, even `"O1" : OptLevel` doesn't typecheck.

It *is* possible to build values which the typechecker will accept as valid
`OptLevel`s, but doing so creates restrictions on how the value can be used
in typed code, so it's not what we want here. For more information see [the
relevant section in the typing documentation].

[the relevant section in the typing documentation]: ./typing.md#using-contracts-as-types

#### Using a contract annotation

For validating custom properties such as `OptLevel`, a contract is the way to
go:

```nickel
# config.ncl
let {OptLevel} = import "lib.ncl" in
let level = 4 in
{
  opt_level | OptLevel = "A" ++ string.from_num level,
}
```

This correctly reports an error, and even gives the computed offending value:

```text
error: contract broken by a value.
  ┌─ :1:1
  │
1 │ OptLevel
  │ -------- expected type
  │
  ┌─ /path/to/config.ncl:4:26
  │
3 │   opt_level | OptLevel = "A" ++ string.from_num level,
  │                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^ applied to this expression
  │
  ┌─ <unknown> (generated by evaluation):1:1
  │
1 │ "A4"
  │ ---- evaluated to this value
```

### Summary

Types and contracts are the two mechanisms to ensure the correctness of Nickel
programs.

Typechecking is a static, ahead-of-time mechanism, which is exhaustive
and reports errors early, but is rigid and limited in the properties which can
be checked.

Contract checking is a runtime mechanism, meaning contracts are more flexible
and able to enforce arbitrary user-defined properties. However, contract
checking alone may report fewer errors than typechecking, and report them later.

As a rule of thumb, types should usually be used for functions operating on
fairly generic data. Contracts work best on data, and are especially useful
for data that will end up in a final configuration. When enforcing custom
properties which are outside the scope of builtin types, contracts work best
for both functions and data.

An in-depth explanation of the type system and how to use it can be found in the
[typing section](./typing.md). For contracts, refer to the [contracts section](./contracts.md).

[^1]: The use of "well-defined" here is somewhat nuanced. Using the Nickel LSP
      server in an editor, for example, does perform checks ahead of time, while
      the program is being written. It is also possible to perform typechecking
      separately before distributing a configuration using `nickel typecheck`.
      However, a raw source program is not guaranteed to have been checked in
      any way prior to execution.
