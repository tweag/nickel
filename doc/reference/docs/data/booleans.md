# Booleans

The boolean values in Nickel are denoted `true` and `false`.

Nickel features the classical boolean operators **and** (`&&`), **or**
(`||`) and **not** (`!`). The **and** and **or** operators are lazy in
the evaluation of the second argument: in `exp1 && exp2`, `exp2` is only
evaluated if `exp1` evaluates to `true`; in `exp1 || exp2`, `exp2` is
only evaluated if `exp1` evaluates to `false`.

Here are some examples of boolean operators in Nickel:

``` { .nickel #repl }
> true && false
false

> false || true
true

> ! true
false
```
