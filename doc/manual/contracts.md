---
slug: contracts
---

# Contracts in Nickel

**Note**: Most of the examples of this document are run in the Nickel REPL. To
start a session, run `nickel repl` in your terminal.

For the motivation behind contracts and a high-level overview of contracts and
types, first read the [correctness](./correctness.md) section.

To a first approximation, contracts are assertions. They check that a value
satisfies some property at run-time. If the test passes, the execution can go on
normally. Otherwise, an error is raised. In Nickel, you can apply a contract
using the `|` operator:

```nickel
let x = (1 + 1 | Number) in x
```

Contracts can also be attached to identifiers in a let binding.
The following is equivalent to the previous example:

```nickel
let x | Number = 1 + 1 in x
```

Contracts can also be attached to record fields:

```nickel
{x | Number = 1 + 1}
```

**Important**: contracts attached to a record field, such as `{x | Number = 1}`,
and a freestanding contract annotation such as `{x = (1 | Number)}` *behave
differently with respect to record merging*. However, as far as runtime checks
are concerned (which is the focus of this document), they are equivalent.

Here, `x` is bound to a `Number` contract. When evaluating `x`, the following steps
are performed:

1. Evaluate `1 + 1`.
2. Check that the result is a number.
3. If it is, return the expression unchanged. Otherwise, raise an error.

```nickel #repl
> 1 + 1 | Number
2

> "a" | Number
error: contract broken by a value
[...]
```

Contracts corresponding to the basic types `Number`, `String`, `Bool` and `Dyn`
are available. `Dyn` is a contract that never fails.

## User-defined contracts

### Preamble: idempotency

As you will see in this section, Nickel contracts are more than mere boolean
validators. They can sometimes modify the checked value for valid reasons
exposed below. However, it's hard (if not impossible) to rightfully limit the
expressive power of contracts to be *reasonable* with respect to modifications.

For Nickel, the notion of reasonable modification is practically defined by
idempotency. An idempotent contract is a contract which gives the same result if
applied once or many times to the same value. That is, for any value,
`MyContract` is idempotent if `value | MyContract` and `value | MyContract |
MyContract` gives the same result. Idempotency sounds like a reasonable property
for a contract, even if it can perform some normalization.

**Nickel makes the assumption that all contracts - including user-defined
contracts - are idempotent**, because the converse would be surprising for
consumers of the contract and because this assumption enables important
optimizations such as contract deduplication. **While non-idempotent contracts
shouldn't wreak havoc on your program, they might lead to surprising results**,
such as a change in behavior e.g. when refactoring a program to a new version
that should be identical, typically because the deduplication optimization
doesn't fire anymore.

### By hand

The ability to check arbitrary properties is where run-time contracts really
shine. Let us see how to define our very own contract. Here is an example of a
custom contract:

```nickel
{
  IsFoo = fun label value =>
    if std.is_string value then
      if value == "foo" then
        value
      else
        std.contract.blame_with_message "not equal to \"foo\"" label
    else
      std.contract.blame_with_message "not a string" label,
}
```

A custom contract is a function of two arguments:

- A `label`. The label is provided by the interpreter and contains tracking
  information for error reporting. The label can be used by the contract to
  customize the error reporting as well.
- The value being checked.

Upon success, the contract must return the original value. We will see
the reason why in the [laziness](#laziness) section. To signal failure,
a custom contract uses `std.contract.blame`. Custom contracts can use
the label to customize error reporting upon failure using the functions
from `std.contract.label`, which set various attributes of the label.
`std.contract.blame_with_message message label` is just shorthand for:

```nickel #parse
label
|> std.contract.label.with_message message
|> std.contract.blame
```

`blame` immediately aborts the execution and reports a contract violation error.

In `IsFoo`, we first test if the value is a string, and then if it is equal to
`"foo"`, in which case the contract succeeds. Otherwise, the contract fails with
appropriate error messages. Let us try:

```nickel #repl
# hide-range{1-10}

> let IsFoo = fun label value =>
    if std.is_string value then
      if value == "foo" then
        value
      else
        std.contract.blame_with_message "not equal to \"foo\"" label
    else
      std.contract.blame_with_message "not a string" label

> 1 | IsFoo
error: contract broken by a value
       not a string
[...]

> "a" | IsFoo
error: contract broken by a value
       not equal to "foo"
[...]

> "foo" | IsFoo
"foo"
```

### With `from_predicate`

Our contract is simple: in the end, it tests the condition `value == "foo"`.
Unfortunately, it has a few cascading ifs that don't look very nice. This is a
necessary evil if you want to provide distinct error messages (`not a string`
and `not a "foo"`). However, one could argue in this case that the information
of the contract's name (which is printed for contract violation errors amongst
other data) is enough to understand the error. In this case, we can write our
contract more succinctly as:

```nickel
{ IsFoo = std.contract.from_predicate ((==) "foo") }
```

`std.contract.from_predicate` takes a predicate (a function of one argument
which returns a boolean) and converts it to a contract. The syntax `(==)` turns
the equality operator `==` into a function, and is shorthand for `fun x y => x
== y`. The partial application `(==) "foo"` is then the function `fun y => "foo"
== y`, which is exactly the condition we want. `from_predicate` is useful to
quickly define contracts based on a boolean condition, and when the contract is
simple enough to not require a custom error message.

Here is an example of a port number contract:

```nickel
{
  Port =
    std.contract.from_predicate
      (
        fun value =>
          std.is_number value
          && std.number.is_integer value
          && value >= 0
          && value <= 65535
      )
}
```

### Parametrized contracts

Let us consider a contract for bounds checking:

```nickel
let Between5And10 =
  std.contract.from_predicate
    (
      fun value =>
        std.is_number value
        && value >= 5
        && value <= 10
    )
in
let Schema = {
  level | Between5And10,
}
in

{ level = 5 } | Schema
```

Now, we add a new field to our schema, which must be between `0` and `1`:

```nickel
let Between5And10 =
  std.contract.from_predicate
    (
      fun value =>
        std.is_number value
        && value >= 5
        && value <= 10
    )
in
let Between0And1 =
  std.contract.from_predicate
    (
      fun value =>
        std.is_number value
        && value >= 0
        && value <= 1
    )
in
let Schema = {
  level | Between5And10,
  strength | Between0And1,
}
in

{
  level = 5,
  strength = 0.5,
} | Schema
```

`Between5And9` and `Between0And1` are awkwardly similar. Because custom
contracts are nothing more than functions, we can use function arguments as
parameters for contracts. Given how partial function application works,
parameters must appear first, before the `label` and `value` arguments:

```nickel
let Between = fun min max =>
  std.contract.from_predicate (fun value =>
    value >= min &&
    value <= max) in
let Schema = {
  level | Between 5 10,
  strength | Between 0 1,
}
in

{
  level = 5,
  strength = 0.5,
} | Schema
```

### Contracts parametrized by contracts

Contracts parametrized by other contracts are not really special amongst
parametrized contracts, but note that although contracts can be functions, we
will see soon that they can be different objects as well. Even with a contract
defined as a function, contract application behaves differently than bare
function application. Thus, when manually handling another contract `Contract`,
do not apply it as a function `Contract label value`, but use
`std.contract.apply Contract label value` instead.

One example of a contract parametrized by another contract is a `Nullable`
contract, that accepts a value that is either null or of some other given
format:

```nickel
let Nullable = fun contract label value =>
  if value == null then
    value
  else
    std.contract.apply contract label value
in
[
  # succeeds
  null | Nullable Number,
  # succeeds
  1 | Nullable Number,
  # fails
  "a" | Nullable Number,
]
```

## Compound contracts

This section describes the different ways of composing existing contracts
together to create new ones.

### Records

Record contracts are fundamental in a configuration language like Nickel. They
encode the data schema of a configuration. A record contract is a record literal
with contract annotations and whose field definitions are usually (but not
necessarily) missing:

```nickel
let Port = Dyn in # hide-line
let Schema = {
  path | String,

  connection
    | {
      server_port | Port,
      host | String,
    }
}
in

{
  path = "/foo/bar",
  connection = {
    server_port =
      if host == "localhost" then
        "8080"
      else
        80,
    host = "localhost",
  }
} | Schema
```

`Schema` is a schema describing a configuration with:

- a field `path` that must be a string
- a sub-record `connection`, which has a `server_port` and a `host` field that
  are respectively a port number and a string.

See how we've combined contracts: while defining the `Schema` record contract,
we wrote another inline record contract for the `connection` field, and we
reused the `Port` contract defined earlier. Composition is this ability of using
existing contracts seamlessly as building blocks for new contracts.

If we export this example to JSON, we get:

```console
$ nickel export config.ncl
error: contract broken by the value of `server_port`
   ┌─ example.ncl:26:7
   │
16 │         server_port | Port,
   │                       ---- expected type
   ·
26 │ ╭       if host == "localhost" then
27 │ │         "8080"
   │ │         ------ evaluated to this expression
28 │ │       else
29 │ │         80,
   │ ╰──────────^ applied to this expression
   │
   ┌─ <unknown> (generated by evaluation):1:1
   │
 1 │ "8080"
   │ ------ evaluated to this value
```

Indeed, our `server_port` is a string, while a number was expected. If we
replace `"8080"` by `8080`, we finally obtain:

```json
{
  "connection": {
    "host": "localhost",
    "server_port": 8080
  },
  "path": "/foo/bar"
}
```

If for a specific use-case port numbers can actually also be specified as
strings, you can amend the `Port` contract to be more permissive and accept strings
representing valid port numbers.

#### Metadata

In addition to defining the contracts of fields, record contracts can also
attach metadata (see [merging](./merging.md)) to fields, such as documentation
or default values:

```nickel #repl
> let Schema = {
    foo
      | doc "This documentation will propagate to the final value!"
      | String
      | default
      = "foo",
    bar | Number,
  }

> let config | Schema = {bar = 2}

> std.serialize 'Json config
"{\n  \"bar\": 2,\n  \"foo\": \"foo\"\n}"

# Don't parse this in tests hide-line
> :query config foo
* contract: String
* default: "foo"
* documentation: This documentation will propagate to the final value!
```

#### Open record contracts

By default, record contracts are closed, meaning that additional fields are forbidden:

```nickel #repl
> let Contract = {foo | String}

> {foo = "a", bar = 1} | Contract
error: contract broken by a value
       extra field `bar`
[...]
```

If you want to allow additional fields, append `, ..` after the last field
definition to define an open contract:

```nickel #repl
> let Contract = {foo | String, ..}

> {foo = "a", bar = 1} | Contract
{ bar = 1, foo | String = "a", .. }
```

#### Giving values to fields

While most record contracts don't have field definitions, they can. In fact,
record contracts are nothing special: any record literal can be used both as a
contract or a normal value. If a field is defined both in the contract and the
checked value, the two definitions will be merged in the final result. For
example:

```nickel #repl
> let Secure = {
    must_be_very_secure | Bool = true,
    data | String,
  }

> std.serialize 'Json ({data = ""} | Secure)
"{\n  \"data\": \"\",\n  \"must_be_very_secure\": true\n}"

> {data = "", must_be_very_secure = false} | Secure
error: non mergeable terms
  ┌─ <repl-input-15>:1:36
  │
1 │  {data = "", must_be_very_secure = false} | Secure
  │                                    ^^^^^    ------ originally merged here
  │                                    │
  │                                    cannot merge this expression
  │
  ┌─ <repl-input-13>:2:34
  │
2 │     must_be_very_secure | Bool = true,
  │                                  ^^^^ with this expression
  │
  = Merge operands have the same merge priority but they can't be combined.
  = Both values are of type Bool but they aren't equal.
  = Bool values can only be merged if they are equal
```

**Warning: `=` vs `|`**

It may be tempting to use `=` instead of  `|` to attach a record contract to a
field. That is, writing `Contract = {foo = {bar | String}}` instead of
`Contract = {foo | {bar | String}}`. When applying this contract, the merging
operator will apply the `String` contract to the field `foo` of the checked value.
At first sight, `=` also fits the bill. However, there are a number of subtle
but potentially surprising differences.

One concerns open contracts. Merging never requires the presence of specific
fields: thus, the contract `{bar | String}` attached to `foo` will actually
behave like an open contract, even if you didn't use `..`. This might or might not
be what you want:

```nickel #repl
> let ContractPipe = {
    sub_field | {foo | String}
  }

> let ContractEq = {
    sub_field = {foo | String}
  }

> {sub_field.foo = "a", sub_field.bar = "b"} | ContractPipe
error: contract broken by the value of `sub_field`
       extra field `bar`
[...]

> {sub_field.foo = "a", sub_field.bar = "b"} | ContractEq
{ sub_field = { bar = "b", foo | String = "a", }, }
```

There are other discrepancies, e.g. when applying the contract to a record with
a `default` annotation on `subfield`. Thus, unless you have a specific use-case
in mind, **you should use `|` instead of `=` when attaching record contracts**.

### Type constructors for contracts

We've already seen that the primitive types `Number`, `String` and `Bool` can be
used as contracts. In fact, any type constructor of the [static type
system](./typing.md) can be used to combine contracts.

#### Array

An array contract checks that the value is an array and applies its parameter
contract to each element:

```nickel #repl
> let VeryBig =
  std.contract.from_predicate (
      fun value =>
        std.is_number value
        && value >= 1000
    )

> [1000, 10001, 2] | Array VeryBig
error: contract broken by a value
  ┌─ <repl-input-21>:1:16
  │
1 │  [1000, 10001, 2] | Array VeryBig
  │                ^          ------- expected array element type
  │                │
  │                applied to this expression
```

#### Functions

A function contract `In -> Out` returns a wrapped function which, for each call,
will check the parameter against the `In` contract and the return value against
the `Out` contract. Put differently, `In` represents *pre-conditions* which must
hold for the parameter, and `Out` represents *post-conditions* which must hold
for the return value of the function.

##### Caller vs callee

Function contracts, as opposed to a contract like `Number`, have the peculiarity
of involving two parties. For example:

```nickel #no-check
let add_semi | String -> String = fun x => x ++ ";" in
add_semi 1

let wrong | String -> String = fun x => 0 in
wrong "a"
```

Both of those examples will fail with a contract violation. But they are
different in nature: in the first one, the function `add_semi` respects its
contract. Whenever `x` is a string, `add_semi` does return a string. Here, the
**caller** (the user of the function) is to blame, and not the **callee** (the
function). This is an important distinction. Say you wrote `add_semi` as part
of a library. A wrong call is a random user of your library, somewhere, misusing
`add_semi`. This is not your responsibility to fix, assuming the contract error
messages are good enough to let users understand the issue quickly.

The second example is the converse. The caller provides a string, as requested
by the contract, but the function returns a number. The blame is on the
**callee**. If you are the library writer who shipped the `wrong` function, this
is a bug you have to fix.

The interpreter automatically performs bookkeeping for function contracts in
order to make this caller/callee distinction:

```nickel #repl
> let add_semi | String -> String = fun x => x ++ ";" in
  add_semi 1
error: contract broken by the caller
[...]

> let wrong | String -> String = fun x => 0 in
  wrong "a"
error: contract broken by a function
[...]
```

##### Higher-order functions

The beauty of function contracts is that they gracefully scale to higher-order
functions as well. Higher-order functions are functions that take other
functions as parameters. Here is an example:

```nickel #repl
> let apply_fun | (Number -> Number) -> Number = fun f => f 0 in
  apply_fun (fun x => "a")
error: contract broken by the caller
  ┌─ <repl-input-24>:1:29
  │
1 │  let apply_fun | (Number -> Number) -> Number = fun f => f 0 in
  │                             ------ expected return type of a function provided by the caller
2 │   apply_fun (fun x => "a")
  │                       --- evaluated to this expression
  │
  ┌─ <unknown> (generated by evaluation):1:1
  │
1 │ "a"
  │ --- evaluated to this value
[...]
```

#### Dictionary

The type constructor `{_ : Contract}` represents a record whose field names are
not constrained but whose field values must satisfy `Contract`. In practice,
`Contract` is applied to each field. Such a contract is useful when using
records as an extensible dictionary, that is a key/value store, where keys are
strings and values satisfy `Contract`, for example:

```nickel #repl
> let occurrences | {_: Number} = {a = 2, b = 3, "!" = 5, "^" = 1} in
  occurrences."!"
5
```

## Laziness

In the [section on writing a custom contract by hand](#by-hand), we noted the
strange fact that a custom contract must return a value, instead of just
returning e.g. a boolean to indicate success or failure. A contract could even
always return `null`, as failure is handled separately by aborting. Moreover,
the contracts we have written so far always returned the original unmodified
value upon success, which doesn't sound very useful: after all, the caller and
the interpreter already had access to this value to begin with.

The motivation for this return value is laziness. Nickel is designed to be
*lazy*, only evaluating values on-demand.

For example, record fields are not evaluated until they are accessed, printed
(in the REPL or as a result of `nickel`), or serialized/exported. Thanks to
laziness, you can for example query specific information on a large
configuration without having to actually evaluate everything. We will use the
always failing contract `std.FailWith` to observe where evaluation takes place:

```nickel #repl
> let config = {
    fail | std.FailWith "ooch" = null,
    data | doc "Some information" = 42
  }

# Don't parse this in tests hide-line
> :query config data
* documentation: Some information

> config.fail
error: contract broken by the value of `fail`
       ooch
[...]
```

See how the command `:query config.data` succeeds, although the field `fail`
causes an error when evaluated.

A large configuration will most probably have a root contract attached,
corresponding to its schema. If this contract checked everything eagerly,
forcing the evaluation of most of the configuration, we would lose the benefits
of laziness. Thus, *we want contracts to be lazy as well*. In particular, a
subcontract attached to a field should only fire when the field is requested.
This is the case with record contracts, and in general with all native contracts
combinators.

### Writing lazy contracts

Imagine we want to write a contract similar to `{_ : Bool}`, that is a
dictionary of booleans, but we also want keys to be number literals (although
represented as strings). A valid value could look like
`{"1": true, "2": false, "10": true}`. If we used boolean predicates as the
default for contracts, it would be impossible to make it lazy: as soon as your
contract is called, you would need to produce a `true` or `false` answer, and
checking that fields are all `Bool` requires evaluating them first.

What we can do is to not perform all the checks right away, but **return a new
value which is wrapping the original value with delayed checks inside**. This
is the rationale behind contracts returning a value. Let us see:

```nickel
{
  NumberBoolDict = fun label value =>
    if std.is_record value then
      let check_fields =
        value
        |> std.record.fields
        |> std.array.fold_left
          (
            fun acc field_name =>
              if std.string.is_match "^\\d+$" field_name then
                acc # unused and always null through iteration
              else
                std.contract.blame_with_message "field name `%{field_name}` is not a number" label
          )
          null
      in
      value
      |> std.record.map
        (
          fun name value =>
            let label_with_msg =
              std.contract.label.with_message "field `%{name}` is not a boolean" label
            in
            std.contract.apply Bool label_with_msg value
        )
      |> std.seq check_fields
    else
      std.contract.blame_with_message "not a record" label
}
```

There is a lot to unwrap here. Please refer to the [syntax section](./syntax.md)
if you have trouble parsing the example. We first check that the value is a
record on line 3. We then define `check_fields` on line 4, an expression that
goes over all the record field names and check that each one is a sequence of
digits. We use a left fold with a dummy `null` accumulator as a way to just
iterate through the array without building up anything interesting.

For laziness, the interesting bit happens here:

```nickel #parse
value
|> std.record.map
  (
    fun name value =>
      let label_with_msg =
        std.contract.label.with_message "field `%{name}` is not a boolean" label
      in
      std.contract.apply Bool label_with_msg value
  )
|> std.seq check_fields
```

This is the final return value of the contract (at least when `value` is a
record). This code takes the original record, and maps a function over it which
substitutes each field for the same value but wrapped in a `Bool` contract.
Because records (and record mapping) are lazy, *this doesn't actually execute
the `Bool` contracts right away*. Each contract will only be run when the
corresponding field will be requested.

Finally, we sequence the result with `check_fields`: if we remove this last line
and return the mapped value directly, because Nickel is lazy and `check_fields`
is not used elsewhere, the check wouldn't actually trigger. Using `seq` first
forces the evaluation of `check_fields` unconditionally, ignores the resulting
value and continues with the second argument (here, our wrapped `value`).

Let us see if we indeed preserved laziness:

```nickel #repl
#hide-range{1-29}

> let NumberBoolDict = fun label value =>
    if std.is_record value then
      let check_fields =
        value
        |> std.record.fields
        |> std.array.fold_left
          (
            fun acc field_name =>
              if std.string.is_match "^\\d+$" field_name then
                acc # unused and always null through iteration
              else
                std.contract.blame_with_message "field name `%{field_name}` is not a number" label
          )
          null
      in
      value
      |> std.record.map
        (
          fun name value =>
            let label_with_msg =
              std.contract.label.with_message "field `%{name}` is not a boolean" label
            in
            std.contract.apply Bool label_with_msg value
        )
      |> std.seq check_fields
    else
      std.contract.blame_with_message "not a record" label

> let config | NumberBoolDict = {
    "1" | std.FailWith "ooch" = null, # same as our previous "fail"
    "0" | doc "Some information" = true,
  }

> config."0"
true
```

Yes! Our contract doesn't unduly cause the evaluation of the field `"1"`. Does
it check anything, though?

```nickel #repl
#hide-range{1-29}

> let NumberBoolDict = fun label value =>
    if std.is_record value then
      let check_fields =
        value
        |> std.record.fields
        |> std.array.fold_left
          (
            fun acc field_name =>
              if std.string.is_match "^\\d+$" field_name then
                acc # unused and always null through iteration
              else
                std.contract.blame_with_message "field name `%{field_name}` is not a number" label
          )
          null
      in
      value
      |> std.record.map
        (
          fun name value =>
            let label_with_msg =
              std.contract.label.with_message "field `%{name}` is not a boolean" label
            in
            std.contract.apply Bool label_with_msg value
        )
      |> std.seq check_fields
    else
      std.contract.blame_with_message "not a record" label

> let config | NumberBoolDict = {
    not_a_number = false,
    "0" | doc "Some information" = false,
  }

> config."0"
error: contract broken by a value
       field name `not_a_number` is not a number
[...]

> let config | NumberBoolDict = {
    "0" | doc "Some information" = "not a boolean",
  }

> config."0"
error: contract broken by a value
       field `0` is not a boolean
[...]
```

It does!

#### Remark: lazy, always?

Note that our check that field names are digits is not lazy in the sense that
even when requesting another field `"0"`, we've already triggered this check for
all field names. When evaluating a record lazily, Nickel must still
determine its structure and in particular the field names. What's not evaluated
right away is the *content* of each field.

Thus, performing the check for field names right away is fine: early error
reporting is a good thing, and the field names are readily available, so we
don't force the evaluation of something that wouldn't be evaluated without the
contract. We could have made this check lazy by putting it inside each field,
together with the `Bool` contract application.

#### Conclusion

Our `NumberBoolDict` contract doesn't perform all the checks needed right away.
Instead, **it returns a new value, which is wrapping the original value with
delayed checks inside**. Doing so preserves laziness of the language and only
triggers the checks when the values are used or exported in a configuration.
This is the reason for contracts to return a value, which must be the original
value with potential delayed checks inside.
