# Nickel Syntax

## Simple types

There are tree basic kind of values in Nickel : 
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

Numbers are also comparable, using the dedicated comparison operators, returning a boolean value :
| Operator | Description      | Example   |
|:--------:|:----------------:|:---------:|
| ==       | Equal            | `5 == 5`  |
| !=       | Not Equal        | `5 != 4`  |
| <        | Smaller than     | `2 < 3`   |
| >        | Greater than     | `1 > -5`  |
| >=       | Greater or Equal | `1 >= 1`  |
| <=       | Smaller or Equal | `-1 <= 6` |


### Boolean values

The boolean values in Nickel are denoted `true` and `false`.

To work with boolean values, we have a the classic logical operators *AND* (&&), *OR* (||) and *NOT* (!).
The _AND_ and _OR_ operators are lazy in the evaluation of the second argument. The truth table below gives the results for using these operators:

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
Strings are enclosed by `" ... "` for a single line string or by `m#" ... "#m` for a multiline string.
They can be concatenated with the operator `++`.
Strings must be UTF-8 valid.

Examples:
```
> "Hello, World!"
"Hello, World!"

> m#"Well, if this isn't a multiline string?
Yes it is, indeed it is"#m
"Well, if this isn't a string?
Yes it is, indeed it is"

> "Hello" ++ "World"
"HelloWorld"
```

## Equality Operators

We have seen that the operators `==` and `!=` works for comparing numbers, but they can actually compare any values together. Two values are equal if they have the same _type_ and the same _value_.

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


## Composite types

### List
Lists allows grouping of values into sequences. They are delimited by `[` and `]`, and elements are separated with `,`.

Examples:
```
[1, 2, 3]
["Hello", "World"]
[1, true, "true"]
[]
```

Lists can be concatenated with the operator `@`:
```
> [1] @ [2, 3]
[ 1, 2, 3 ]
```

### Record
Records are key-value storage. They are delimited with by `{` and `}`, elements are separated by `,`.
key-value elements are noted as `key = value`.
The keys can be either identifiers, or strings. Values can be of any type.
Elements inside a record are unordered.

Examples:
```
{}
{a = 3}
{my_id_n5 = "my id number 5", my_id_n4 = }
{"5" = 5, six = 6}
```

A record cannot contain the same key twice, or the values must be identical (see section about _merge_).
```
> {a = 3, a = 3}
{ a = 3 }

> {a = 3, "a" = 3}
{ a = 3 }
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

## Statements

### If-Then-Else
This statement allows conditional branching in your code. You can use it as `if <bool expr> then <expr> else <expr>`. Both expressions in the `then` and `else` branches are lazily evaluated, and must have the same type.

Examples:
```
> if true then "TRUE :)" else "false :("
"TRUE :)"

> if false then "Not this one" else "This one"
"This one"

> if "forty-two" == 42 then "equal?" else "unequal"
"unequal"
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
A function is declared using the `fun` keyword, then arguments separated by spaces, and finally an arrow `=>` to add the body of the function.
To call a function, just add the arguments after it separated with spaces.
Functions in Nickel are curried, meaning that a function taking multiple arguments is actually a function that takes a single argument and returns a function taking the rest of the arguments, until it is applied.

Example
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

