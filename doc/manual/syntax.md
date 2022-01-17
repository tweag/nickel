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
<!-- TODO -->

### List
<!-- TODO -->

### Record
<!-- TODO -->

### Dynamic Record
<!-- TODO -->

### Enum
<!-- TODO -->

## Functions
<!-- TODO -->

## Statements

### If-Then-Else

### Let-In

### Switch
