---
slug: syntax
---

# Nickel Syntax

## Identifiers

Nickel identifiers start with an alphabetic character, followed by zero or more
alphanumeric characters, `_` (underscores) or `'` (single quotes). For example,
`this-isn't-invalid` is a valid identifier.

## Simple values

There are four basic kinds of values in Nickel :

1. numeric values
2. boolean values
3. strings
4. enum tags

### Numeric values

Nickel has support for numbers, positive and negative, with or without
decimals. Internally, those numbers are stored as arbitrary precision rationals,
meaning that basic arithmetic operations (addition, subtraction, division and
multiplication) don't incur rounding errors. Numbers are deserialized as 64-bit
floating point numbers, in line with common JSON implementations.

Epxonentation is supported using the `std.number.pow` function. If the exponent
is exactly representable as an integer between `-2^63` and `2^64 - 1`, the
result is computed exactly. However, raising a number to a non-integral power
can incur rounding errors: both operands will be converted to the nearest 64-bit
floating point numbers, the power is computed as a 64-bit floating point number
as well, and then converted back to an arbitrary precision rational number.

Numbers are serialized as integers whenever possible, that is, when they
fit exactly into a 64-bit signed integer or a 64-bit unsigned integer. They
are serialized as a 64-bit float otherwise. The latter conversion might lose
precision as well, for example when serializing `1/3`.

Here are some examples of number literals in Nickel:

```nickel
1
0.543
42
-1000000
-6.8
```

There are some predefined operators for working with numbers:
| Operator | Description                                          | Example       |
|:--------:|:----------------------------------------------------:|:-------------:|
| +        | The addition operator                                | `1 + 2 = 3`   |
| \-       | The subtraction operator                             | `1 - 2 = -1`  |
| *        | The multiplication operator                          | `1 * 2 = 2`   |
| /        | The division operator                                | `1 / 2 = 0.5` |
| %        | The modulo operator (returns the *signed* remainder) | `5 % 3 = 2`   |

> **Remark about the `-` operator:** Since `-` can be used inside an identifier,
> the subtraction operators **needs** to be surrounded by spaces: write `a - b`,
> not `a-b`. `1-2` works as expected, because `1` and `2` aren't identifiers.

Numbers can be compared using the following operators:
| Operator | Description      | Example   |
|:--------:|:----------------:|:---------:|
| ==       | Equal            | `5 == 5`  |
| !=       | Not Equal        | `5 != 4`  |
| <        | Smaller than     | `2 < 3`   |
| >        | Greater than     | `1 > -5`  |
| >=       | Greater or Equal | `1 >= 1`  |
| <=       | Smaller or Equal | `-1 <= 6` |

In the table below, you will find the operators sorted from highest to lowest precedence:
|       Operators      | Associativity | Remark                                        |
|:--------------------:|:-------------:|-----------------------------------------------|
|       `( ... )`      |               | parentheses always have the highest precedence |
|          `-`         |               | unary negation (as in `-1`)                   |
|     `*`, `/`, `%`    | left-to-right |                                               |
|       `+`, `-`       | left-to-right | binary addition and subtraction               |
| `<`, `>`, `=<`, `>=` | left-to-right |                                               |
|      `==`, `!=`      | left-to-right |                                               |

### Boolean values

The boolean values in Nickel are denoted `true` and `false`.

Nickel features the classical boolean operators *AND* (&&), *OR* (||) and *NOT*
(!). The *AND* and *OR* operators are lazy in the evaluation of the second
argument: for example, in `exp1 && exp2`, `exp2` is only evaluated if `exp1`
evaluates to `false`.

Here are some examples of boolean operators in Nickel:

```nickel
> true && false
false

> false || true
true

> ! true
false
```

### Strings

Nickel can work with sequences of characters, or strings. Strings are enclosed
by `" ... "` for a single line string or by `m%" ... "%` for a multiline string.
They can be concatenated with the operator `++`. Strings must be UTF-8 valid.
In fact, as far as at all practicable, Nickel treats strings as sequences of
Unicode extended grapheme clusters and refuses to break them apart.

The string interpolation syntax is
`"%{ < expression that evaluates to a string > }"`.

Here are some examples of string handling in Nickel:

```nickel
> "Hello, World!"
"Hello, World!"

> m%"Well, if this isn't a multiline string?
Yes it is, indeed it is"%
"Well, if this isn't a multiline string?
Yes it is, indeed it is"

> "Hello" ++ "World"
"HelloWorld"

> let h = "Hello" in "%{h} World"
"Hello World"

> let n = 5 in "The number %{n}."
error: Type error

> let n = 5 in "The number %{std.string.from_number n}."
"The number 5."
```

Multiline strings are useful for writing indented lines. The first and last
lines are ignored if they are empty or contain only spaces. Indentation that is
present on all lines of the string is stripped. This way, multiline strings can
be indented for nicer code formatting without producing unwanted whitespace in
the output. For example:

```nickel
> m%"
  This line has no indentation.
    This line is indented.
      This line is even more indented.
  This line has no more indentation.
"%
"This line has no indentation.
  This line is indented.
    This line is even more indented.
This line has no more indentation."
```

The only special sequence in a multiline string is the string interpolation:

```nickel
> m%"Multiline\nString?"%
"Multiline\nString?"

> m%"Multiline%{"\n"}String"%
"Multiline
String"
```

A multiline string can be introduced and closed with multiple `%` signs, as
long as the number of `%` signs in the start delimiter equals the number in the
closing delimiter. If you want to use string interpolation, you must use the
same amount of `%` signs as in the delimiters. This can be useful for escaping
`"%` or `%{` sequences in a string:

```nickel
> m%%"Hello World"%%
"Hello World"

> m%%%%%"Hello World"%%%%%
"Hello World"

> let w = "World" in m%%"Hello %{w}"%%
"Hello %{w}"

> let w = "World" in m%%"Hello %%{w}"%%
"Hello World"
```

Multiline string interpolation is "indentation-aware". This means that you can
interpolate a string with indentation and the result will be as expected:

```nickel
> let log = m%"
  if log:
    print("log:", s)
  "% in m%"
  def concat(str_array, log=false):
    res = []
    for s in str_array:
      %{log}
      res.append(s)
    return res
  "%
"def concat(str_array, log=false):
  res = []
  for s in str_array:
    if log:
      print("log:", s)
    res.append(s)
  return res"
```

#### Symbolic Strings

Some tools targeted by Nickel require manipulating string-like values that are
not yet known at the time of evaluation, such as Terraform's computed values.
Others, like Nix, perform additional dependency tracking (see [Nix string
context][nix-string-context]). In both cases, we have to build and combine
string-like values which are more complex than bare strings, but for which using
a string syntax would still feel natural.

That is precisely the use-case for symbolic strings:

```nickel
{
  args = [
      "-c",
      nix-s%"
        %{inputs.gcc}/bin/gcc %{inputs.hello} -o hello
        %{inputs.coreutils}/bin/mkdir -p $out/bin
        %{inputs.coreutils}/bin/cp hello $out/bin/hello
      "%,
  ],
  ..
}
```

This example is an excerpt of a Nix configuration written in Nickel, emulating
Nix string contexts. The region delimited by `nix-s%"` and `"%"` on lines 4 to
8 is a symbolic string. The values `inputs.gcc`, `inputs.hello`, etc. aren't
actually strings, but arbitrary records, because they carry additional context.
Yet, they can be interpolated as if they were strings.

The idea behind symbolic strings is to offer a string-like syntax, but without
evaluating the expression as a string. Instead, the expression is returned in a
symbolic form - in practice, an array of fragments, where each fragment is
either a string or an arbitrary value that has been interpolated - and Nickel
lets the specific library (Terraform-Nickel, Nix-Nickel, etc.) handle it.

The prefix of a symbolic string is any valid identifier that doesn't start with
`_`, and ends with the suffix `-s`. Prefixes don't have any meaning for Nickel:
they're just a tag used by libraries consuming symbolic strings to distinguish
between several types of symbolic strings. Prefixes are also a visual marker for
the programmer.

Besides the custom prefix, symbolic strings otherwise follow the same syntactic
rules as multiline strings: the prefix is followed by an arbitrary number of `%`
followed by `"`, and must be closed by `"` followed by the same number of `%`.

The technical details don't matter too much in practice. As a user of a library
which uses symbolic strings, remember that:

- a special string with a prefix ending in  `-s` is a symbolic string. The
  prefix (or prefixes) is defined by the library.
- it's a special syntax without pre-existing meaning for Nickel. The
  specific meaning of each kind of symbolic string, and what it's used for
  exactly, is defined by the library. All in all, symbolic strings simply
  provide libraries with a way to overload string syntax and interpolation for
  extended usages.
- the main operation supported by symbolic strings is interpolation: `%{value}`.
  What interpolation means, and which values can be interpolated in a given
  symbolic string is again defined by each library. Other string functions don't
  work on symbolic strings (e.g. `std.string.length`, `std.string.characters`,
  and so on), because they might not have any valid meaning. Instead, libraries
  should export their own string API, if they support additional operations on
  their symbolic strings.

The following examples show how symbolic strings are desugared:

```nickel
> mytag-s%"I'm %{"symbolic"} with %{"fragments"}"%
{
  tag = 'SymbolicString,
  prefix = 'mytag
  fragments = [ "I'm ", "symbolic", " with ", "fragments" ],
}

> let terraform_computed_field = {
    tag = 'TfComputed,
    resource = "foo",
    field = "id",
  }
> tf-s%"id: %{terraform_computed_field}, port: %{5}"%
{
  tag = 'SymbolicString
  prefix = 'tf,
  fragments = [ "id: ", { resource = "foo", field = "id", tag = 'TfComputed }, ", port: ", 5 ],
}
```

#### Enum tags

Enumeration tags are used to express a choice among finitely many alternatives.
They are formed by writing a single quote `'` followed by any valid identifier
or by a quoted string. For example, `std.serialize` takes an export format as a
first argument, which is an enum tag among `'Json`, `'Toml` or `'Yaml`:

```nickel
> std.serialize 'Json {foo = 1}
"{
   \"foo\": 1
 }"

> std.serialize 'Toml {foo = 1}
"foo = 1
"
```

An enum tag `'foo` is serialized as the string `"foo"`:

```nickel
> std.serialize 'Json {foo = 'bar}
"{
  \"foo\": \"bar\"
}"
```

While it's technically possible to just use strings in place of enum tags, using
an enum tag encodes the intent that only a finite number of alternatives can be
used for the corresponding value.

Additionally, the typechecker is aware of enums and can for example statically
enforce that only valid tags are passed to a function within a typed block. See
[the manual section on typing](./typing.md) for more details.

## Equality

Operators `==` and `!=` are used to compare values. Two values of different
types are never equal: that is, `==` doesn't perform implicit conversions.

Here are some examples of equality comparisons in Nickel:

```nickel
> 1 == 1
true

> 5 == 5.0
true

> "Hello" == "Hello"
true

> "Hello" != "World"
true

> 5 == "Hello"
false

> true == "true"
false
```

## Composite values

### Arrays

An array is a sequence of values. Arrays are delimited by `[` and `]`, and
elements are separated with `,`.

The following are valid Nickel arrays, for example:

```nickel
[1, 2, 3]
["Hello", "World"]
[1, true, "true"]
[]
```

Arrays can be concatenated with the operator `@`:

```nickel
> [1] @ [2, 3]
[ 1, 2, 3 ]
```

### Records

Records are key-value storage, or in Nickel terms, field-value storage. They
are delimited by `{` and `}`, and elements are separated with `,`. A field
definition is written as `field = value`. The fields are strings, but can be
written without quotes `"` if they are valid identifiers. Values can be of
any type. Elements inside a record are unordered. Two records can be *merged*
together using the operator `&`. The reader can find more information about
merging in the [section on merging](./merging.md).

Here are some valid Nickel records:

```nickel
{}
{a = 3}
{my_id_n5 = "my id number 5", "my id n4" = "my id number 4" }
{"5" = 5, six = 6}
```

Record fields can be accessed using the `.` operator :

```nickel
> { a = 1, b = 5 }.a
1

> { a = 1 }.b
error: Missing field

> { "1" = "one" }."1"
"one"
```

It is possible to write records of records via *piecewise syntax*, where we
separate fields by dots:

```nickel
> { a = { b = 1 } }
{ a = { b = 1 } }

> { a.b = 1 }
{ a = { b = 1 } }

> { a.b = 1, a.c = 2, b = 3}
{ a = { b = 1, c = 2 }, b = 3 }
```

When fields are enclosed in double quotes (`"`), you can use string
interpolation to create or access fields:

```nickel
> let k = "a" in { "%{k}" = 1 }
{ a = 1 }

> let k = "a" in { a = 1 }."%{k}"
1
```

## Constructs

### If-Then-Else

This construct allows conditional branching in your code. You can use it like
`if <bool expr> then <expr> else <expr>`.

Here are some valid conditional expressions in Nickel:

```nickel
> if true then "TRUE :)" else "false :("
"TRUE :)"

> if false then "Not this one" else "This one"
"This one"

> if "forty-two" == 42 then "equal?" else "unequal"
"unequal"

> ["1"] @ (if 42 == "42" then ["3"] else ["2"]) @ ["3"]
["1", "2", "3"]
```

### Let-In

A `let` binds an expression to a variable. It is used like `let <rec?>
<ident> = <expr> in <expr>`. The `rec` keyword makes the binding recursive,
enabling the use of `<ident>` within the bound
expression `<expr>`.
Currently, only a single variable can be bound per let binding.

Here are some examples of let bindings in Nickel:

```nickel
> let r = { a = "a", b = "b" } in r.a
"a"

> let inner = { inside = true } in let outer = { outside = inner.inside } in outer.outside
true

> let a = 1 in let b = 2 in a + b
3

> let rec f = fun n => if n == 0 then n else n + f (n - 1) in f 10
55

> let rec fib = fun n => if n <= 2 then 1 else fib (n - 1) + fib (n - 2) in fib 9
34

> let rec repeat = fun n x => if n <= 0 then [] else repeat (n - 1) x @ [x] in
    repeat 3 "foo"
["foo", "foo", "foo"]
```

## Functions

A function is declared using the `fun` keyword, then arguments separated with
spaces, and finally an arrow `=>` to add the body of the function. To call a
function, just write the arguments after it separated with spaces. Functions in
Nickel are curried: a function taking multiple arguments is actually a function
that takes a single argument and returns a function taking the rest of the
arguments, and so on.

Here are some examples of function definitions in Nickel:

```nickel
> (fun a b => a + b) 1 2
3

> let add = fun a b => a + b in add 1 2
3

> let add = fun a b => a + b in
    let add1 = add 1 in
        add1 2
3
```

All existing infix operators in Nickel can be turned into functions by putting
them inside parentheses, for example:

```nickel
> 1 + 2
3

> (+) 1 2
3

> let increment = fun n => (+) 1 n in
    increment 41
42

> let increment = (+) 1 in
    increment 41
42

> let flatten = std.array.fold_right (@) [] in
    flatten [[1, 2], [3], [4, 5]]
[ 1, 2, 3, 4, 5 ]
```

Functions may be composed using the *pipe operator*. The pipe operator allows
for a function application `f x` to be written as `x |> f`. This operator is
left-associative, so `x |> f |> g` will be interpreted as `g (f x)`. For example:

```nickel
> "Hello World" |> std.string.split " "
["Hello", "World"]

> "Hello World"
  |> std.string.split " "
  |> std.array.first
"Hello"

> "Hello World"
  |> std.string.split " "
  |> std.array.first
  |> std.string.uppercase
"HELLO"
```

## Annotations

Contract and type annotations help enforce additional properties of an
expression. They can be attached to any Nickel expression. See [the correctness
section](./correctness.md) for more details.

### Type annotations

A type annotation is introduced using `<expr> : <type>` and serves to delimit
a statically typed block which will be checked by the typechecker before
evaluation. A type annotation can be directly attached to the variable of a let-
binding `let <var> : <type> = <expr> in <body>` or to a record field declaration
`{<field> : <type> = <value>}` as well.

A type wildcard `_` indicates that part of a type is unknown to the user (or
is not worth spelling out). The typechecker will attempt to infer it. Adding a
wildcard type annotation `: _` to an existing expression is particularly useful
for debugging, as it's the simplest way to have the typechecker run on an
expression without having to come up with a type.

Here are some examples of type annotations in Nickel:

```nickel
> 5 : Number
5

> "hello" : String
"hello"

> "Hello," ++ " world!" : String
"Hello, world!"

> 5 + "a" : _
error: incompatible types
[..]

> (1 + 1 : Number) + ((`foo |> match { `foo => 1, _ => 2 }) : Number)
3

> let x : Number = "a" in x
error: incompatible types
[..]

> let complex_argument : _ -> Number = fun {field1, field2, field3} => field1 in
    complex_argument {field1 = 5, field2 = null, field3 = false}
5
```

### Contract annotations

A contract annotation is introduced by `<exp> | <contract>` and serves to apply
a runtime check to an expression (among other things).

As detailed in the next section, `<expr>`, `<type>` and `<contract>` are in fact
syntactically all the same and can be arbitrary Nickel expressions in practice.

Here are some examples of contract annotations in Nickel:

```nickel
> 5 | Number
5

> 5 | Bool
error: contract broken by a value.
[..]

> let SmallNumber = std.contract.from_predicate (fun x => x < 5) in
1 | SmallNumber
1

> let SmallNumber = std.contract.from_predicate (fun x => x < 5) in
10 | SmallNumber
error: contract broken by a value.
[..]

> let SmallNumber = std.contract.from_predicate (fun x => x < 5) in
  let NotTooSmallNumber = std.contract.from_predicate (fun x => x >= 2) in
  3 | Number
    | SmallNumber
    | NotTooSmallNumber
3
```

### Types

The Nickel syntax mixes both terms and types in the same namespace. The
following program is perfectly legal: `let value = Number -> (fun value label =>
value) in ((fun x => x + 1) : value)`. See the
[RFC002](https://github.com/tweag/nickel/blob/d723a3721c6b0fe9c4b856e889bb7211d6136665/rfcs/002-merge-types-terms-syntax.md)
for a detailed account of this design.

The documentation still makes a distinction between *types* and other
expressions, the former being constructs which are handled specially by the
typechecker and are listed below. However, any expression can be considered a
type (in the generic case, it will be considered as an opaque type), and type
constructors can also appear inside an expression (where they are understood as
their associated contract, which is indeed an expression, most often a
function).

Thus, placeholders such as `<source>`, `<target>` or `<type>` can actually be
substituted with any valid Nickel expression (which includes the type
constructors we've just listed), and types can appear anywhere.

Nickel features the following builtin types and type constructors:

- Primitive types: `Number`, `String`, `Bool`, and `Dyn` (the dynamic type, wich
represents any value)
- Arrays: `Array <type>` is an array whose elements are of type `<type>`.
- Dictionaries: `{_ : <type>}` is a record whose fields are of type `<type>`.
- Enums: `[| 'tag1, .., 'tagn |]` is an enumeration comprised of the alternatives
  `'tag1`, .., `'tagn`. Tags have the same syntax as identifiers and must
  be prefixed with a single quote `'`. Like record fields, they can however be
  enclosed in double quotes if they contain special characters:
  `'"tag with space"`.
- Arrows: `<source> -> <target>` is a function taking an argument of type
  `<source>` and returns values of type `<target>`.
- Foralls: `forall var1 .. varn. <type>` is a polymorphic type quantifying over type
variables `var1`, .., `varn`.
- Records: see the next section [Record types](#record-types).

Type variables bound by a `forall` are only visible inside types (any of the
constructor listed above). As soon as a term expression appears under a `forall`
binder, the type variables aren't in scope anymore:

```nickel
> forall a. a -> (a -> a) -> {_ : {foo : a}}
<func>

> forall a. a -> (a -> (fun x => a))
error: unbound identifier
  ┌─ repl-input-4:1:32
  │
1 │ forall a. a -> (a -> (fun x => a))
  │                                ^ this identifier is unbound
```

Here are some examples of more complicated types in Nickel:

```nickel
> let f : forall a. a -> a = fun x => x in (f 5 : Number)
5

> {foo = [[1]]} : {foo : Array (Array Number)}
{ foo = [ [ 1 ] ] }

> let select
    : forall a. {left: a, right: a} -> [| 'left, 'right |] -> a
    = fun {left, right} =>
       match {
         'left => left,
         'right => right,
       }
  in
  (select {left = true, right = false} 'left) : Bool
true

> let add_foo : forall a. {_: a} -> a -> {_: a} = fun dict value =>
    record.insert "foo" value dict
in
add_foo {bar = 1} 5 : _
{ bar = 1, foo = 5 }

> {foo = 1, bar = "string"} : {_ : Number}
error: incompatible types
  ┌─ repl-input-12:1:17
  │
1 │ {foo = 1, bar = "string"} : {_ : Number}
  │                 ^^^^^^^^ this expression
  │
  = The type of the expression was expected to be `Number`
  = The type of the expression was inferred to be `String`
  = These types are not compatible
```

#### Record types

Record types are syntactically a restricted subset of record literals. They are
handled differently than normal record literals with respect to typechecking.

A record literal is a record type if:

- No field has a defined value: there are only fields without definition.
- Each field has exactly one type annotation
- Each field doesn't have any other metadata attached (see [Metadata](#metadata))

If these properties are satisfied, a record literal is considered to be a
record type by the typechecker.

A record literal which is interpreted as a record type may have a *record
tail*. A tail is written like `{ <fields> ; <tail> }`. It appears at the end of
the field declarations and is preceded by `;`. The tail `<tail>` itself must be
a valid identifier.

Trying to attach a tail `; tail` to a record literal which isn't a record type
is a parse error.

Here are some examples of record types in Nickel:

```nickel
> {foo = 1, bar = "foo" } : {foo : Number, bar: String}
{ bar = "foo", foo = 1 }

> {foo.bar = 1, baz = 2} : {foo: {bar : Number}, baz : Number}
{ baz = 2, foo = { bar = 1 } }
```

Here, the right-hand side is missing a type annotation for `baz`, so it doesn't
qualify as a record type and is parsed as a record contract. This throws an
"incompatible types" error:

```nickel
> {foo = 1, bar = "foo" } : {foo : Number, bar : String, baz}
error: incompatible types
  ┌─ repl-input-6:1:1
  │
1 │ {foo = 1, bar = "foo" } : {foo : Number, bar : String, baz}
  │ ^^^^^^^^^^^^^^^^^^^^^^^ this expression
  │
  = The type of the expression was expected to be `{ bar : String, baz, foo : Numb…` (a contract)
  = The type of the expression was inferred to be `{bar: _a, foo: _b}`
  = Static types and contracts are not compatible
```

If there's a metadata annotation apart from the type, the record cannot be
parsed as a type. Consequently, it is considered a record contract and converted
into an opaque type, yielding an error:

```nickel
> {foo = 1, bar = "foo" } : {foo : Number, bar : String | optional}
error: incompatible types
[..]
```

While in the following `MyDyn` isn't a proper type, the record literal `{foo :
Number, bar : MyDyn}` respects all the requirements for a record type and is
parsed as such:

```nickel
> let MyDyn = fun label value => value in
    {foo = 1, bar | MyDyn = "foo"} : {foo : Number, bar : MyDyn}
{ bar = "foo", foo = 1 }
```

## Metadata

Metadata annotations are used to attach type and contract annotations,
documentation, a merge priority or other decorations to record fields (and
record fields only). Multiple metadata annotations can be chained. Metadata
is introduced with the syntax `<field_name> | <metadata1> | .. | <metadataN>
[= value]`.

Documentation can be attached with `| doc <string>`. For example:

```nickel
> let record = {
    value
      | doc "The number five"
      | default = 5
  }
> :query record value
• default: 5
• documentation: The number five

> {
    truth
      | Bool
      | doc m%"
          If something is true,
          it is based on facts rather than being invented or imagined,
          and is accurate and reliable.
          (Collins dictionary)
        "%
      = true,
  }
{ truth = true }
```

Metadata can also set merge priorities using the following annotations:

- `default` is the lowest priority, usually used for default values that are
  expected to be overridden somewhere
- `priority NN`, where `NN` is a number literal, is a numeral priority
- `force` is the highest priority

If there is no priority specified, `priority 0` is the default. See more
about this in the [dedicated section on merging](./merging.md).

Here are some examples using merge priorities in Nickel:

```nickel
> let Ais2ByDefault = { a | default = 2 } in
    {} | Ais2ByDefault
{ a = 2 }

> let Ais2ByDefault = { a | default = 2 } in
    { a = 1 } | Ais2ByDefault
{ a = 1 }

> { foo | default = 1, bar = foo + 1 }
{ foo = 1, bar = 2 }

> {foo | default = 1, bar = foo + 1} & {foo = 2}
{ foo = 2, bar = 3 }

> {foo | force = 1, bar = foo + 1} & {foo = 2}
{ bar = 2, foo = 1 }

> {foo | priority 10 = 1} & {foo | priority 8 = 2} & {foo = 3}
{ foo = 1 }

> {foo | priority -1 = 1} & {foo = 2}
{ foo = 2 }
```

The `optional` annotation indicates that a field is not mandatory. It is usually
found in record contracts.

```nickel
> let Contract = {
    foo | Num,
    bar | Num
        | optional,
  }
> let value | Contract = {foo = 1}
> value
{ foo = 1 }

> {bar = 1} | Contract
error: missing definition for `foo`
[..]
```

The `not_exported` annotation indicates that a field should be skipped when a
record is serialized. This includes the output of the `nickel export` command:

```nickel
> let value = { foo = 1, bar | not_exported = 2}
> value
{ foo = 1, bar = 2 }

> std.serialize 'Json value
"{
  "foo": 1
}"
```

[nix-string-context]: https://shealevy.com/blog/2018/08/05/understanding-nixs-string-context/
