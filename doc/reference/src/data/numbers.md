# Numbers
<!-- toc -->

## Number literals

Number literals in Nickel can be written in:

- decimal representation, like `123`
- hexadecimal representation with a leading `0x`, like `0xFF15a` (the characters
  `a` through `f` are case-insensitive)
- octal representation with a leading `0o`, like `0o777`
- binary representation with a leading `0b`, like `0b00101`.

The decimal representation supports scientific notation and decimal points,
like `3e-3`, `1.2`, or `1.2e3`. The hexadecimal, octal, and binary representations
do not; `0xfa.fa` is an error.

In some contexts, negative number literals can be written without a parenthesis,
as in `let x = -1 in x * x`. In other contexts, the negative sign will be
parsed as a binary negation operator: `std.number.pow 1 -1` is parsed as
`std.number.pow (1 - 1)`. To parse the negative sign as a unary negation
in contexts like this, add parentheses: `std.number.pow 1 (-1)`.

## Numeric representation

All numbers in Nickel are arbitrary-precision rational numbers, meaning that basic
arithmetic operations (addition, subtraction, division, and multiplication)
never overflow or incur rounding errors. Numbers in Nickel cannot represent
positive or negative infinity, or [NaN](https://en.wikipedia.org/wiki/NaN).

The standard library contains some numeric functions (like `std.number.pow`
or `std.number.cos`) which may introduce rounding errors.

## Built-in numeric operators

There are some predefined operators for working with numbers:

| Operator | Description                                          | Example       |
|:--------:|:----------------------------------------------------:|:-------------:|
| +        | The addition operator                                | `1 + 2 = 3`   |
| \-       | The subtraction operator                             | `1 - 2 = -1`  |
| *        | The multiplication operator                          | `1 * 2 = 2`   |
| /        | The division operator                                | `1 / 2 = 0.5` |
| %        | The modulo operator (returns the *signed* remainder) | `5 % 3 = 2`   |

> **Remark about the `-` operator:** Since `-` can be used inside an identifier,
> the subtraction operator **needs** to be surrounded by spaces: write `a - b`,
> not `a-b`. `1-2` works as expected, because identifiers can't start with a digit.

## Built-in numeric comparators

Numbers can be compared using the following operators:

| Operator | Description      | Example   |
|:--------:|:----------------:|:---------:|
| ==       | Equal            | `5 == 5`  |
| !=       | Not Equal        | `5 != 4`  |
| <        | Smaller than     | `2 < 3`   |
| >        | Greater than     | `1 > -5`  |
| >=       | Greater or Equal | `1 >= 1`  |
| <=       | Smaller or Equal | `-1 <= 6` |

## Operator precedence

In the table below, you will find the operators sorted from highest to lowest precedence:

|       Operators      | Associativity | Remark                                         |
|:--------------------:|:-------------:|------------------------------------------------|
|       `( ... )`      |               | parentheses always have the highest precedence |
|          `-`         |               | unary negation (as in `-1`)                    |
|     `*`, `/`, `%`    | left-to-right |                                                |
|       `+`, `-`       | left-to-right | binary addition and subtraction                |
| `<`, `>`, `=<`, `>=` | left-to-right |                                                |
|      `==`, `!=`      | left-to-right |                                                |
