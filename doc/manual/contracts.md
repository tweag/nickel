# contracts

Aside of types, to constraint variables in nickel, there are contracts.
Contracts are more powerfull than types in the behaviours perspective.
The difference between contracts and types which, in some cases benefits more
to types than to contracts is the time of application and check. Basicaly,
types are checked before evaluation and are exaustives when contracts are checked
during evaluation only on evaluated values.
You can read (Types VS Contracts)["./types_vs_contracts.md"] to learn how to
chose between these.

## Simple form (builtins contracts)

The simplest form of contracts is actualy using types:

```text
{
    port | Num,
    host | Str,
}
```
Here, the contracts will check the types at evaluation. If they pass, caller
is sure that the fields have these types. Actualy, during evaluation, even
static types are treated as contracts.

## Contracts from predicates

There are two ways to write custom contracts. The first and simple one is to
write them like a boolean predicate.

```text
let Port = fun p => contracts.from_predicate
  builtins.is_num p &&
  p % 1 == 0 &&
  p >= 0 &&
  p <= 65535) in
{
  port | #Port = 80,
}
```

Here, during evaluation, the contract will check that `port` is a natural
positive number smaller or equal to 65535.

As you can see, a custom contract application is preceded by a `#`.
Anyway this specificity is subject to be droped in a near futur.

## Fully custom contracts

The next step to write contracts is to write an actualy custom contract.
As the previous case, these are funcions but with some differences:

- They take a label param in addition to the value param.
- They return a blame instead of simply a boolean.

The main benefits, as can be seen in the following exemple, is to blame with
a custom message.


The following example is actualy a simple contract extracted from the stdlib `string`

```text
let BoolLiteral = fun lbl s =>
  if %is_str% s then
    if s == "true" || s == "True" then
      "true"
    else if s == "false" || s == "False" then
      "false"
    else
      contracts.blame (contracts.tag "expected \"true\" or \"false\", got #{s}" lbl)
  else
    contracts.blame (contracts.tag "not a string" lbl)
```

Here, `lbl` is the blame label. It contains several info. Mainly position from
where contract was called and an error message.

As you see, to set the error message of a label, we used the `contracts.tag` function.
This way is the more flexible one, because it let you tag a label one time
and blame in various places.
Anyway, an other way to write the same contract blaming in one pass is using
`contracts.blame_with` function:

```text
let BoolLiteral = fun lbl s =>
  if %is_str% s then
    if s == "true" || s == "True" then
      "true"
    else if s == "false" || s == "False" then
      "false"
    else
      contracts.blame_with "expected \"true\" or \"false\", got #{s}" lbl
  else
    contracts.blame_with "not a string" lbl
```

For this case, the second version is a bit shorter and may be more readable.
