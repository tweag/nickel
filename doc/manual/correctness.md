Nickel features two different mechanisms to help you write correct
configurations: *types* and *contracts*. In essence, their fundamental purpose
is the same: to ensure that an expression verifies some desired properties. And
if it doens't, it should fail with an informative error message. The range of
the properties that you can test is wide, for example:

- To evaluate to a number
- To evaluate to a valid port number
- To evalute to the same value as this other field `foo` of the same configuration
- To be a function always sending numbers to numbers
- etc.

Both types and contracts are specified similarly too, using an annotation:

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

**The fundamental difference is that type annotations are checked statically,
before the program even run, while contracts are checked at run-time**. The
basic characteristics and use-cases pretty much all follow from this basic fact.

## Motivation

Usually, static typing brings in important benefits for large codebases of
general-purpose programming languages, but the case of an interpreted
configuration language appears less clear-cut.

For pure configuration code, which is mostly data, static typing is not as
useful. First, a configuration is a terminating program run once on fixed
inputs: here, basic type errors will show up at runtime anyway. Second, our
definition of correctness also includes validating the generated configuration.
For this usage, static types are too rigid. Statically checking that an
expression will always evaluate to a valid port number, for example, requires
advanced machinery. On the other hand, checking this property at runtime is
trivial.

Nevertheless, if you have ever faced puzzling [dynamic type
errors](https://www.haskellforall.com/2021/01/dynamic-type-errors-lack-relevance.html),
you may have felt the need for something better. Classic dynamic typing is prone
to error messages being unrelated to the actual issue and pointing to a location
far from the offending code. This is especially salient when working with
functions, which tend to delay type errors by passing around ill-formed values
until they eventually break evaluation somewhere else. For reusable code, i.e.
functions, static typing really helps.

This apparent dilemma is solved in Nickel by supporting *gradual typing*.
Gradual typing enables to mix both static typing and dynamic typing. What's
more, the dynamic typing is augmented by contracts, which provides a principled
and ergonomic way of checking arbitrary assertions.

You'll find a in-depth description of the type system and how to use it in the
[typing section](./typing.md). For contracts, see how to validate your
configuration in the [contract section](./contracts.md). Finally, if you are
looking for a quick cheat-sheet about when to use static typing or contracts,
please visit [Type versus contracts: when to?](./types-vs-contracts.md).
