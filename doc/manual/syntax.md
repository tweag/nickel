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

Nickel has a support for numbers, positive and negative, with or without
decimals. Internally, those numbers are stored as 64-bits floating point
numbers, following the IEEE 754 standard.

Examples:

```nickel
1
0.543
42
-1000000
-6.8
```

There are a some predefined operators for working with numbers :
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

Numbers can be compared using the following operators :
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

Examples:

```text
> true && false
false

> false || true
true

> ! true
false
```

### Strings

Nickel can work with sequences of characters, or strings. Strings are enclosed
by `" ... "` for a single line string or by `m%" ... "%` for a multiline
string. They can be concatenated with the operator `++`. Strings must be UTF-8
valid.

The string interpolation syntax is
`"%{ < expression that evaluates to a string > }"`.

Examples:

```text
> "Hello, World!"
"Hello, World!"

> m%"Well, if this isn't a multiline string?
Yes it is, indeed it is"%
"Well, if this isn't a string?
Yes it is, indeed it is"

> "Hello" ++ "World"
"HelloWorld"

> let h = "Hello" in "%{h} World"
"Hello World"

> let n = 5 in "The number %{n}."
error: Type error

> let n = 5 in "The number %{string.from_num n}."
"The number 5."
```

Multiline strings are useful to write indented lines. The indentation is
stripped from the beginning of the first line, and first and last lines are
ignored if they are empty or contain only spaces.

Example:

```text
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

The only special sequence in a multiline string is the string interpolation.

Examples:

```text
> m%"Multiline\nString?"%
"Multiline\nString?"

> m%"Multiline%{"\n"}String"%
"Multiline
String"
```

A multiline string can be introduced and closed by multiple `%` signs, as long
as this amount is equal. If you want to use string interpolation, you must use
the same amount of `%` as in the delimiters.

Examples:

```text
> m%%"Hello World"%%
"Hello World"

> m%%%%%"Hello World"%%%%%
"Hello World"

> let w = "World" in m%%"Hello %{w}"%%
"Hello %{w}"

> let w = "World" in m%%"Hello %%{w}"%%
"Hello World"
```

Multiline strings are "indentation-aware". This means that one could use an
indented string interpolation and the indentation would behave as expected:

```text
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
a string syntax would still be natural.

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
Nix string context. Lines 4 to 8 define a symbolic string. Values `inputs.gcc`,
`inputs.hello`, etc. aren't actually strings, but arbitrary records, because
they carry additional context. Yet, they can be interpolated as if they were
strings.

The idea behind symbolic strings is to offer a string-like syntax, but without
evaluating the expression as a string. Instead, the expression is returned in a
symoblic form - in practice, an array of fragments, where each fragment is
either a string or an arbitrary value that has been interpolated - and Nickel
lets the specific library (Terraform-Nickel, Nix-Nickel, etc.) handle it.

The prefix of a symbolic string is any valid identifier that doesn't start with
`_`, and ends with the suffix `-s`. Prefixes don't have any meaning for Nickel:
they're a tag used by libraries consuming symbolic strings to distinguish
between several types of symbolic strings. Prefixes are also a visual marker for
the programmer.

Beside the custom prefix, symbolic strings otherwise follow the same syntactic
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
  work on symbolic strings (e.g. `string.length`, `string.characters`, and so
  on), because they might not have any valid meaning. Instead, libraries should
  export their own string API, if they support additional operations on their
  symbolic strings.

The following examples show how symbolic strings are desugared:

```text
> mytag-s%"I'm %{"symbolic"} with %{"fragments"}"%
{
  tag = `SymbolicString,
  prefix = `mytag
  fragments = [ "I'm ", "symbolic", " with ", "fragments" ],
}

> let terraform_computed_field = {
    tag = `TfComputed,
    resource = "foo",
    field = "id",
  }
> tf-s%"id: %{terraform_computed_field}, port: %{5}"%
{
  tag = `SymbolicString
  prefix = `tf,
  fragments = [ "id: ", { resource = "foo", field = "id", tag = `TfComputed }, ", port: ", 5 ],
}
```

#### Enum tags

Enumeration tags are used to express finite alternatives. They are formed by
writing a backtick `` ` `` followed by any valid identifier. For example,
`builtin.serialize` takes an export format as a first argument, which is an enum
tag among `` `Json ``, `` `Toml `` or `` `Yaml `` (as of version 0.1):

```nickel
builtin.serialize `Json {foo = 1}
# gives "{
#          \"foo\": 1
#        }"
builtin.serialize `Toml {foo = 1}
# gives "foo = 1
#       "
```

An enum tag `` `foo `` is serialized as the string `"foo"`:

```nickel
let as_yaml_string = builtin.serialize `Yaml {foo = `bar}
# gives "---
#       foo: bar
#       "

```

While it's technically possible to just use strings in place of enum tags, using
an enum tag insists on the fact that only a finite number of alternatives can be
used for the corresponding value.

Additionally, the typechecker is aware of enums and can for example statically
enforce that only valid tags are passed to a function within a typed block. See
[the manual section on typing](./typing.md) for more details.

## Equality

Operators `==` and `!=` are used to compare values. Two values of different
types are never equal: that is, `==` doesn't perform implicit conversions.

Examples:

```text
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

### Array

An array is a sequence of values. They are delimited by `[` and `]`, and
elements are separated with `,`.

Examples:

```nickel
[1, 2, 3]
["Hello", "World"]
[1, true, "true"]
[]
```

Arrays can be concatenated with the operator `@`:

```text
> [1] @ [2, 3]
[ 1, 2, 3 ]
```

### Record

Records are key-value storage, or in Nickel terms, field-value storage. They are
delimited by `{` and `}`, and elements are separated with `,`. Field-value
elements are noted as `field = value`. The fields are strings, but can be
written without quotes `"` if they respect identifiers syntax. Values can be of
any type. Elements inside a record are unordered. Two records can be *merged*
together using the operator `&`. The reader can find more information about
merging in the [section on merging](./merging.md).

Examples:

```nickel
{}
{a = 3}
{my_id_n5 = "my id number 5", "my id n4" = "my id number 4" }
{"5" = 5, six = 6}
```

Accessing a record field can be done using the `.` operator :

```text
> { a = 1, b = 5 }.a
1

> { a = 1 }.b
error: Missing field

> { "1" = "one" }."1"
"one"
```

It is possible to write records of records via the *piecewise syntax*, where we
separate fields by dots:

```text
> { a = { b = 1 } }
{ a = { b = 1 } }

> { a.b = 1 }
{ a = { b = 1 } }

> { a.b = 1, a.c = 2, b = 3}
{ a = { b = 1, c = 2 }, b = 3 }
```

When fields are enclosed with double quotes (`"`), you can use string
interpolation to create or access fields:

```text
> let k = "a" in { "%{k}" = 1 }
{ a = 1 }

> let k = "a" in { a = 1 }."%{k}"
1
```

## Constructs

### If-Then-Else

This construct allows conditional branching in your code. You can use it as
`if <bool expr> then <expr> else <expr>`.

Examples:

```text
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

Let-in allows the binding of an expression. It is used as
`let <rec?> <ident> = <expr> in <expr>`. The `rec` keyword in Let-in constructs
allows the let binding to become recursive, enabling the use of the `<ident>`
within the first `<expr>`.

Examples:

```text
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
function, just add the arguments after it separated with spaces. Functions in
Nickel are curried, meaning that a function taking multiple arguments is
actually a function that takes a single argument and returns a function taking
the rest of the arguments, until it is applied.

Examples:

```text
> (fun a b => a + b) 1 2
3

let add = fun a b => a + b in add 1 2
3

> let add = fun a b => a + b in
    let add1 = add 1 in
        add1 2
3
```

All existing infix operators in Nickel can be turned into functions by putting
them inside parentheses.

Examples:

```text
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

> let flatten = array.fold_right (@) [] in flatten [[1, 2], [3], [4, 5]]
[ 1, 2, 3, 4, 5 ]
```

Functions might be composed using the *pipe operator*. The pipe operator allows
for a function application `f x` to be written as `x |> f`. This operator is
left-associative, so `x |> f |> g` will be interpreted as `g (f x)`.

Examples:

```text
> "Hello World" |> string.split " "
["Hello", "World"]

> "Hello World"
  |> string.split " "
  |> array.first
"Hello"

> "Hello World"
  |> string.split " "
  |> array.first
  |> string.uppercase
"HELLO"
```

## Typing

To give a type to a value, we write it with `< value > : < type >`. More
information on typing in the [relevant document](./typing.md).

Examples:

```nickel
5 : Num
"Hello" : Str

(fun a b => a + b) : Num -> Num -> Num
let add : Num -> Num -> Num = fun a b => a + b

{a: Num = 1, b: Bool = true, c : Array Num = [ 1 ]}
let r : {a : Num, b : Bool, c : Array Num} = { a = 1, b = true, c = [ 1 ] }

{ a = 1, b = 2 } : { _ : Num }
let r : { _ : Num } = { a = 1, b = 2 }
```

## Metadata

Metadata are used to attach contracts (more information in relevant
documentation), documentation or priority to values. A metadata is introduced
with the syntax `<value> | <metadata>`. Multiple metadata can be chained.

Examples:

```text
> 5 | Num
5

> 5 | Bool
error: contract broken by a value.
[..]

> let SmallNum = contract.from_predicate (fun x => x < 5) in
1 | SmallNum
1

> let SmallNum = contract.from_predicate (fun x => x < 5) in
10 | SmallNum
error: contract broken by a value.
[..]

> let SmallNum = contract.from_predicate (fun x => x < 5) in
  let NotTooSmallNum = contract.from_predicate (fun x => x >= 2) in
  3 | Num
    | SmallNum
    | NotTooSmallNum
3
```

Adding documentation can be done with `| doc < string >`. Examples:

```text
> 5 | doc "The number five"
5

> true | Bool | doc m%"
    If something is true,
    it is based on facts rather than being invented or imagined,
    and is accurate and reliable.
    (Collins dictionary)
    "%
true
```

Metadata can also set merge priorities using the following annotations:

- `default` gives the lowest priority (default values)
- `priority NN`, where `NN` is a number literal, gives a numeral priority
- `force` gives the highest priority

If there is no priority specified, `priority 0` is given by default. See more
about this in the dedicated section on merging.

Examples:

```text
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

The `optional` metadata indicates that a field is not mandatory, and is mostly
used inside record contracts:

```text
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

The `not_exported` metadata indicates that a field shouldn't appear in the
serialization (including the output of the `nickel export` command):

```text
> let value = { foo = 1, bar | not_exported = 2}
> value
{ foo = 1, bar = 2 }

> builtin.serialize `Json value
"{
  "foo": 1
}"
```

[nix-string-context]: https://shealevy.com/blog/2018/08/05/understanding-nixs-string-context/
