# Correctness

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
3. Informative error messages .

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
of properties that can be tested, for example:

- To evaluate to a number.
- To evaluate to the same value as this other field `foo` of the same configuration.
- To evaluate to a record with at least one field `port` that is a valid port number.
- To be a function always mapping numbers to numbers.
- etc.

Both types and contracts are enforced in a similar way, using an annotation:

```nickel
>1 + 1.5 : Num

2.5
> let f : Num -> Num = fun x => x + 1
> f 0

1
> let GreaterThan = fun bound =>
    contracts.from_predicate (fun val => val >= bound) in
-1 | GreaterThan 10

error: Blame error (contract broken by value)
[..]
```

`:` introduces a type annotation, while `|` introduces a contract application.
They support the same syntax for properties on the right-hand side.

**The fundamental difference between types and contracts is that type
annotations are checked statically, before the program even start, while
contracts are checked lazily at run-time**. The characteristics and use-cases of
types and contracts directly follow from this distinction.

For example, using a custom property like `GreaterThan 10` as a type annotation
won't be very useful, as the typechecker doesn't know much about it. It won't be
able to statically enforce it and will be overly restrictive in what you can do
with it. For this kind of checks, contracts are the tool of choice. On the other
hand, using a function contract only tests a function on a finite number of
inputs, while static typing is able to prove properties for all possible inputs.

As a rule of thumb, you should use type for functions and contracts for data
(records and lists), especially data that ends up in the final configuration.

You'll find a in-depth description of the type system and how to use it in the
[typing section](./typing.md). For contracts, refer to the
[contract section](./contracts.md). Finally, if you are looking for a quick
cheat-sheet about when to use static typing or contracts, go to
[Type versus contracts: when to?](./types-vs-contracts.md).

[^1]: This statement is to be nuanced. Using the Nickel LSP server in your
  editor, for example, does perform checks a head of type, while you are typing
  program. You can also perform typechecking separately before distributing a
  configuration using `nickel typecheck`. However, a source program is not
  guaranteed to have been checked in any way before execution.
