# Contracts

To first approximation, contracts are assertions. They check that a value
satisfies some property at run-time. If the test passes, the execution can go on
normally. Otherwise, an error is raised. In Nickel, you can enforce a contract
using the `|` operator:

```nickel
let x = (1 + 1 | Num) in x
```

Here, `x` is bound to a `Num` contract. When evaluating `x`, the following steps
are performed:

1. Evaluate `1 + 1`.
2. Check that the result is a number.
3. If it is, return the expression unchanged. Otherwise, raise an error.

```sh
$nickel repl
nickel>1 + 1 | Num
2

nickel>"a" | Num
error: contract broken by a value.
[..]
```

Contracts corresponding to the native types `Num`, `Str` and `Bool` are available.

## User-defined contracts

### By hand

Having only those native contracts would be very limiting. Being checked at
run-time, contracts have the ability to enforce arbitrary properties easily. Let
us see how to define our very own contract. We start the REPL (`nickel repl`) and
input:

```nickel
let IsFoo = fun label value =>
  if builtins.is_str value then
    if value == "foo" then
      value
    else
      contracts.blame_with "not equal to foo" label
  else
    contracts.blame_with "not a string" label
```

A custom contract is a function of two arguments:

- A `label`. Provided by the interpreter, the label contains tracking information
  for error reporting. Its main usage is to be passed to `contracts.blame` or
  `contracts.blame_with` when the contract isn't satisfied.
- The tested value.

Upon success, the contract must return the original value. It may seem strange
at first, but we will see later why just using a boolean is not an option.

In our case, we first test if the value is a string, and then if it is equal to
`"foo"`, in which case the contract succeeds. Otherwise, it fail with
appropriate error messages. Let us try:

```
nickel>1 | IsFoo
error: contract broken by a value [not a string].
[..]

nickel>"a" | IsFoo
error: contract broken by a value [not equal to foo].
[..]

nickel>"foo" | IsFoo
"foo"
```

### With `from_predicate`

Our contract is simple: in the end, it tests the condition `value == "foo"`.
Unfortunately, it has a few cascading ifs that don't look very nice. This is a
necessary evil if you want to provide distinct error messages (`not a string`
and `not a "foo"`). However, one could argue in this case that the information
of the contract's name is enough to understand the error. In this case, we can
write our contract more succintly as:

```nickel
let IsFoo = contracts.from_predicate ((==) "foo")
```

`contracts.from_predicate` takes a predicate (a function of one parameter that
returns a boolean) and converts it to a contract. Here, the syntax `(==)` turns
an operator into a function, and is a shorthand for `fun x y => x == y`. The
partial application to `"foo"` is then the function `fun y => "foo" == y`, which
is exactly the condition we want. `from_predicate` is useful to quickly define
contracts based on a series of boolean conditions, and where the contract is
simple enough such that errors doesn't need more detailed reports.

Here is an example of a port number contract:

```nickel
let Port = contracts.from_predicate (fun value =>
  builtins.is_num value &&
  nums.is_int value &&
  value >= 0 &&
  value <= 65535)
```

## Compound contracts

The appeal of contract, over say just using bare functions and a simple abort
primitive, is that they can be nicely composed to build larger contracts.

### Records

Record contracts are fundamental in a configuration like Nickel. They can
represent data schema. A record contract is just a record literal with contract
annotations, but whose field definitions are missing:

```nickel
let MyConfig = {
  path | Str,
 
  connection | {
    server_port | Port,
    host | Str,
  }
}
```

This is a schema describing a configuration with:

- a field `path` that must be a string
- a sub-record `connection`, which has a `server_port` and `host` field that
  are respectively a port number and a string.

See how we've composed contracts: while defining the `MyConfig` record contract,
we wrote another inline record contract for the `connection` field, and we also
reused the `Port` contract defined above. Composition menas we can use existing
contracts as building blocks to build new ones.

### Others



## Laziness
