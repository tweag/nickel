# Nickel Syntax

## Simple values

There are three basic kind of values in Nickel : 
 1. numeric values, 
 2. boolean values,
 3. strings.
 
### Numeric values

Nickel has a support for numbers, positive and negative, with or without decimals.
Internally, those numbers are stored as 64-bits floating point numbers, following the IEEE 754 standard.

Examples:

```
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

> **Remark about the `-` operator:**  
> Since `-` can be used inside an identifier, the subtraction operators **needs** to be surrounded by spaces:
> write `1 - 1`, not `1-1`.

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

Nickel features the classical boolean operators *AND* (&&), *OR* (||) and *NOT* (!).
The _AND_ and _OR_ operators are lazy in the evaluation of the second argument: for example, in `exp1 && exp2`, `exp2` is only evaluated if `exp1` evaluates to `false`.

Examples:
```
> true && false
false

> false || true
true

> ! true
false
```

### Strings

Nickel can work with sequences of characters, or strings.
Strings are enclosed by `" ... "` for a single line string or by `m%" ... "%m` for a multiline string.
They can be concatenated with the operator `++`.
Strings must be UTF-8 valid.

The string interpolation syntax is `"%{ < expression that evaluates to a string > }"`.

Examples:
```
> "Hello, World!"
"Hello, World!"

> m%"Well, if this isn't a multiline string?
Yes it is, indeed it is"%m
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

Multiline strings are useful to write indented lines. The indentation is stripped from the beginning of the first line, and first and last lines are ignored if they are empty or contain only spaces.

Example:
```
> m%"
This line has no identation.
  This line is indented.
    This line is even more indented.
This line has no more identation.
"%m
"This line has no indentation.
  This line is indented.
    This line is even more indented.
This line has no more indentation." 
```

The only special sequence in a multiline string is the string interpolation.

Examples:
```
> m%"Multiline\nString?"%m
"Multiline\nString?"

> m%"Multiline%{"\n"}String"%m
"Multiline
String"
```

A multiline string can be introduced and closed by multiple `%` signs, as long as this amount is equal. If you want to use string interpolation, you must use the same amount of `%` as in the delimiters.

Examples:
```
> m%%"Hello World"%%m
"Hello World"

> m%%%%%"Hello World"%%%%%m
"Hello World"

> let w = "World" in m%%"Hello %{w}"%%m
"Hello %{w}"

> let w = "World" in m%%"Hello %%{w}"%%m
"Hello World"
```

Multiline strings are "indentation-aware". This means that one could use an indented string interpolation and the indentation would behave as expected:

```
> let log = m%"
if log:
  print("log:", s)
"%m in m%"
def concat(str_array, log=false):
  res = []
  for s in str_array:
    %{log}
    res.append(s)
  return res
"%m
"def concat(str_array, log=false):
  res = []
  for s in str_array:
    if log:
      print("log:", s)
    res.append(s)
  return res"
```

## Equality

Operators `==` and `!=` are used to compare values. Two values of different types are never equal: that is, `==` doesn't perform implicit conversions.

Examples:
```
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
An array is a sequence of values. They are delimited by `[` and `]`, and elements are separated with `,`.

Examples:
```
[1, 2, 3]
["Hello", "World"]
[1, true, "true"]
[]
```

Arrays can be concatenated with the operator `@`:
```
> [1] @ [2, 3]
[ 1, 2, 3 ]
```

### Record
Records are key-value storage, or in Nickel terms, field-value storage. They are delimited by `{` and `}`, and elements are separated with `,`.
Field-value elements are noted as `field = value`.
The fields are strings, but can be written without quotes `"` if they respect identifiers syntax. Values can be of any type.
Elements inside a record are unordered.
Two records can be _merged_ together using the operator `&`. The reader can find more information about merging in the relevant documentation.

Examples:
```
{}
{a = 3}
{my_id_n5 = "my id number 5", "my id n4" = "my id number 4" }
{"5" = 5, six = 6}
```

Accessing a record field can be done using the `.` operator :
```
> { a = 1, b = 5 }.a
1

> { a = 1 }.b
error: Missing field

> { "1" = "one" }."1"
"one"
```

It is possible to write records of records via the *piecewise syntax*, where we separate fields by dots:
```
> { a = { b = 1 } }
{ a = { b = 1 } }
 
> { a.b = 1 }
{ a = { b = 1 } }

> { a.b = 1, a.c = 2, b = 3}
{ a = { b = 1, c = 2 }, b = 3 }
```

When fields are enclosed with double quotes (`"`), you can use string interpolation to create or access fields:
```
> let k = "a" in { "%{k}" = 1 }
{ a = 1 }

> let k = "a" in { a = 1 }."%{k}"
1
```

## Constructs

### If-Then-Else
This construct allows conditional branching in your code. You can use it as `if <bool expr> then <expr> else <expr>`.

Examples:
```
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
Let-in allows the binding of an expression. It is used as `let <ident> = <expr> in <expr>`.

Examples:
```
> let r = { a = "a", b = "b" } in r.a
"a"

> let inner = { inside = true } in let outer = { outside = inner.inside } in outer.outside
true

> let a = 1 in let b = 2 in a + b
3
```

## Functions
A function is declared using the `fun` keyword, then arguments separated with spaces, and finally an arrow `=>` to add the body of the function.
To call a function, just add the arguments after it separated with spaces.
Functions in Nickel are curried, meaning that a function taking multiple arguments is actually a function that takes a single argument and returns a function taking the rest of the arguments, until it is applied.

Examples:
```
> (fun a b => a + b) 1 2
3

let add = fun a b => a + b in add 1 2
3

> let add = fun a b => a + b in
    let add1 = add 1 in
        add1 2
3
```

All existing infix operators in Nickel can be turned into functions by putting them inside parentheses.

Examples:
```
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

> let flatten = array.fold (@) [] in flatten [[1, 2], [3], [4, 5]]
[ 1, 2, 3, 4, 5 ]
```

Functions might be composed using the *pipe operator*. The pipe operator allows for a function application `f x` to be written as `x |> f`.
This operator is left-associative, so `x |> f |> g` will be interpreted as `g (f x)`.

Examples:
```
> "Hello World" |> string.split " "
["Hello", "World"]

> "Hello World"
  |> string.split " "
  |> array.head
"Hello"

> "Hello World"
  |> string.split " "
  |> array.head
  |> string.uppercase
"HELLO"
```

## Typing
To give a type to a value, we write it with `< value > : < type >`.
More information on typing in the relevant document.

Examples:
```
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
Metadata are used to attach contracts (more information in relevant documentation), documentation or priority to values.
A metadata is introduced with the syntax `<value> | <metadata>`. Multiple metadata can be chained.


Examples:
```
> 5 | Num
5

> 5 | Bool
error: Blame error: contract broken by a value.

> let SmallNum = contract.from_predicate (fun x => x < 5) in
1 | SmallNum
1

> let SmallNum = contract.from_predicate (fun x => x < 5) in
10 | SmallNum
error: Blame error: contract broken by a value.

> let SmallNum = contract.from_predicate (fun x => x < 5) in
  let NotTooSmallNum = contract.from_predicate (fun x => x >= 2) in
  3 | Num
    | SmallNum
    | NotTooSmallNum
3
```

Adding documentation can be done with `| doc < string >`.  
Examples:
```
> 5 | doc "The number five"
5

> true | Bool | doc m%"
    If something is true,
    it is based on facts rather than being invented or imagined,
    and is accurate and reliable.
    (Collins dictionary)
    "%m
true
```

Record contracts can set default values using the `default` metadata:
It is noted as `| default = < default value >`.
This is especially useful when merging records (more about this in the dedicated document about merge).  
Examples:
```
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
```
