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

The ability to check arbitrary properties is where run-time contracts really
shine. This section explains how to define and use your very own contract.

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
doesn't fire anymore. You must always ensure that you write idempotent
contracts.

### Predicate

The simplest way to write a custom contract is to use
`std.contract.from_predicate`. This function takes a predicate, which is a
function taking a value and returning a boolean (that is, a function of type
`Dyn -> Bool`) and converts it to a contract.

Here is a contract that checks if a string is equal to `"foo"`:

```nickel
{ IsFoo = std.contract.from_predicate ((==) "foo") }
```

The syntax `(==)` turns the equality operator `==` into a function, and is
shorthand for `fun x y => x == y`. The partial application `(==) "foo"` is then
the function `fun y => "foo" == y`, which is exactly the condition we want.

Here is another example of a port number contract:

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

One drawback of `std.contract.from_predicate` is that it doesn't allow for
custom error messages. `from_predicate` is useful to quickly define contracts
based on a boolean condition and when the contract is simple enough to not
require a custom error message: as for primitive types, just seeing the name of
the contract should be sufficient for other developers to understand what went
wrong.

### Validators

Predicates are useful for simple contracts, but they have an important
shortcoming: it's impossible to customize the reported error messages in case of
contract violation. Validators are an enhanced version of predicates that can
include additional information to be reported in case of error. They leverage
Nickel's enum variants to do so.

Instead of returning a boolean, a validator returns a value of type:

```nickel
[|
  'Ok,
  'Error {
    message
      | String
      | optional,
    notes
      | Array String
      | optional,
  }
|]
```

This type includes the value `'Ok` to signal success and `'Error data` to signal
a violation, where `data` is a record that has an optional field `message`
representing the main error explanation and an optional field `notes`, which is
an array of strings, for additional notes included at the end of the error
message. Each note spans a new line.

Let's rewrite the `IsFoo` contract using a validator with more precise error
reporting:

```nickel #repl
> let IsFoo =
  std.contract.from_validator
      (
        match {
          "foo" => 'Ok,
          value if std.is_string value =>
            'Error {
              message = "expected \"foo\", got \"%{value}\"",
            },
          value =>
            let typeof = value |> std.typeof |> std.to_string in
            'Error {
              message = "expected a String, got a %{typeof}",
              notes = ["The value must be a string equal to \"foo\"."],
            },
        }
      )

> 1 | IsFoo
error: contract broken by a value
       expected a String, got a Number
  ┌─ <repl-input-3>:1:2
  │
1 │  1 | IsFoo
  │  ^   ----- expected type
  │  │
  │  applied to this expression
  │
  = The value must be a string equal to "foo".

> "a" | IsFoo
error: contract broken by a value
       expected "foo", got "a"
[...]

> "foo" | IsFoo
"foo"
```

### General custom contracts

In some situations, even validators aren't sufficient. For example, when writing
[delayed contracts](#delayed-contracts) or [parametrized
contracts](#parametrized-contracts), you might not be able to decide right away
if a value satisfies a contract.

For those use-cases, the stdlib provides the additional contract constructor
`std.contract.custom` which is the most general form (predicates and validators
are just special cases of general custom contracts).

The argument of `std.contract.custom` must be a function that takes two
arguments:

- A `label`.
- The value being checked.

This function must return either `'Ok new_value` or `'Error {..}`, almost like a
validator, but with the additional `new_value` which is the original value with
potential delayed checks included.

#### Label

For immediate checks, signaling failure should be done by returning `'Error`, as
for validators, whenever possible. However, for delayed checks, we must do
differently. A custom contract uses `std.contract.blame` which takes the label
as an argument. `blame` immediately aborts the execution and reports a contract
violation error. You can think of it as throwing an exception, but one that
can't be caught.

The label is a special object which is automatically passed to the custom
contract by the interpreter. The label contains tracking information for error
reporting. Custom contracts can use the label to customize error reporting upon
failure using the functions from `std.contract.label`, which set various
attributes of the label and return the new modified label. As seen in the
[contracts parametrized by contracts
section](#contracts-parametrized-by-contracts), the label must also be provided
when applying a subcontract.

The use of labels is illustrated in the [delayed contracts
section](#delayed-contracts). Another example of a general custom contracts is
given in the next section on parametrized contracts.

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

`Between5And9` and `Between0And1` are awkwardly similar. Instead, we can use a
single function parametrized by some arguments which returns a contract:

```nickel
let Between = fun min max =>
  std.contract.from_predicate (fun value =>
    value >= min &&
    value <= max)
in

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
parametrized contracts: it's just that the additional argument turns out to be a
contract as well. Usually, a contract taking a contract parameter will
eventually apply it, using either `std.contract.apply` or
`std.contract.check` (the difference between these is explained in the
next subsection).

Parameters that are also contracts may contain delayed checks. In
consequence, a contract parametrized by another unknown contract must usually be
written as a [custom contract](#general-custom-contracts) as well, using the
constructor `std.contract.custom`.

One example is a `Nullable` contract, which accepts a value that is either
`null` or of some other given format. If the argument is `null`, we accept the
value immediately (and return it unchanged). Otherwise, we proceed with the
argument contract.

```nickel
let Nullable = fun Contract =>
  std.contract.custom
    (fun label value =>
      if value == null then
        'Ok value
      else
        std.contract.check Contract label value
    )
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

#### `apply` vs `check`

The stdlib provides two variants for applying a contract. The difference lies in
their return value, and how they propagate errors.

`std.contract.apply` is used by the interpreter when evaluating a contract
application such as `value | Contract`. As such, a contract violation is always
turned into a call to `std.contract.blame` and aborts the execution immediately.
In particular, even an immediate error can't be caught from normal Nickel code.
If the contract succeeds, or at least the immediate part, the value with
potential delayed check inside is returned directly.

When implementing a parametrized contract, `apply` should be used for [delayed
checks](#delayed-contracts). Here is a contrived example re-implementing the
builtin contract `[| 'Foo Contract |]` parametrized by `Contract`:

```nickel #repl
> let FooOf = fun Contract =>
  std.contract.custom (fun label => match {
    'Foo arg => 'Ok ('Foo (std.contract.apply Contract label arg)),
    _ => 'Error {},
  })

> 'Foo 5 | FooOf Number
'Foo 5

> 'Foo "a" | FooOf Number
error: contract broken by a value
[...]
```

In this case, `Contract` is applied as part of the delayed checks. When those
checks are eventually run, we aren't in the context of the implementation of a
contract anymore, and `'Ok` or `'Error` aren't meaningful. We need to either
abort upon failure, or to proceed transparently with the evaluation of `value`.

On the other hand, `std.contract.check` should be used in the
situation where a parametrized contract performs some immediate checks and then
completely transfers the execution to another contract. This is precisely the
case of the `Nullable` example above:

```nickel #repl
> let Nullable = fun Contract =>
    std.contract.custom
      (fun label value =>
        if value == null then
          'Ok value
        else
          std.contract.check Contract label value
      )

> null | Nullable Number
null

> 5 | Nullable Number
5

> "a" | Nullable Number
error: contract broken by a value
[...]
```

In this case, we do want the contract application to return either `'Ok` or
`'Error`. Indeed, we don't know yet if we should return `'Ok` or not: `Contract`
might itself returns an immediate error through `'Error` which we would like to
bubble up as an immediate error too.

`check` preserves immediate errors, while `apply` converts them to
blame errors (it also spares us from wrapping the result in `'Ok` but this is
more anecdotal). This makes more contract errors catchable.

For example, with the current version, `Nullable Number` is an immediate
contract that returns `'Error` if the value is neither `null` nor a `Number`
(say, `"a"`). We can in particular use it with boolean contract combinators,
such as in `std.contract.any_of [Nullable Number, String]`, with the expected
behavior. Had we used `std.contract.apply` in this case, this would turn the
`Number` sub-check to a delayed check, meaning that `"a" | std.contract.any_of
[Nullable Number, String]` would now error out unduly. See [boolean
combinators](#boolean-combinators)] for more details on `any_of`.

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
  ┌─ <repl-input-22>:1:36
  │
1 │  {data = "", must_be_very_secure = false} | Secure
  │                                    ^^^^^    ------ originally merged here
  │                                    │
  │                                    cannot merge this expression
  │
  ┌─ <repl-input-20>:2:34
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
  ┌─ <repl-input-28>:1:16
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
  ┌─ <repl-input-31>:1:29
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

## Delayed contracts

This section covers delayed contracts, why they are useful, and how to write
them. You might come across the term *lazy contracts* elsewhere in the
documentation, which was the previous denomination of delayed contracts: the two
notions are strictly equivalent.

### Lazy evaluation

In the [section on writing a custom contract with
`std.contract.custom`](#general-custom-contracts), we noted the surprising fact
that a general custom contract must return `'Ok new_value`, instead of just
returning `'Ok`.

The motivation for this return value is laziness. Nickel is designed to be
*lazy*, only evaluating values on-demand.

When evaluating a record, Nickel only evaluates the *spine* of the record, that
is the structure of the record (the field names and their metadata), but not its
content. The field values are left unevaluated, until they are accessed, printed
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

A large configuration usually has a root contract attached, corresponding to its
schema. If this contract would perform all the checks immediately, forcing the
evaluation of most of the configuration, we would lose the benefits of laziness.
Thus, *we want contracts to be lazy as well*. In particular, when writing a
contract `{foo | FooContract}`, we want that `FooContract` fires only when the
field `foo` is requested. This is indeed the case: built-in contract
combinators, such as array contracts, dictionary contracts and function
contracts, all have a *delayed part*.

### Immediate and delayed

It's useful to see a general contract as having two parts:

- An **immediate part** which is evaluated first and produces an answer right
    away. This answer is either:
  - `'Ok new_value` to signal success of the immediate checks, returning
      the delayed part in `new_value`
  - `'Error {..}` to signal immediate failure
- A **delayed part** included in `new_value` which corresponds to the
    original value with delayed checks integrated inside. Those checks will fire
    only when further data is requested. Throwing an error from the delayed part
    is done using `std.contract.blame`, which is more like throwing an
    exception, as opposed to simply returning `'Error` in the immediate part.

In practice, a general custom contract is just one function combining both. The
immediate/delayed distinction is just conceptual, but it's still a helpful point
of view to understand the design of Nickel's contract system.

Take the record contract `{foo | FooContract}` applied to the value `{foo =
42}`:

- the immediate part of this contract will check that the value is a record and
    that its only field is `foo`, which is the case.
- the delayed part will push the `FooContract` contract inside the value and
    return the result, giving `{foo | FooContract = 42}`.

A contract built from a predicate or a validator (using respectively
`std.contract.from_predicate` and `std.contract.from_validator`) only has an
immediate part.

### Writing a delayed contract

First, let us mention that writing a contract with a delayed part is not that
common. Predicates and validators should cover most of your needs. Delayed
contracts are most often native Nickel contract combinators. It's still possible
to need one's own specialized variant of built-in contracts, which will be our
working example in this section.

Another use-case for custom delayed contracts is to write a contract that is
parametrized by another contract, such as the `Nullable` contract implemented in
[the parametrized contracts section](#contracts-parametrized-by-contracts).
Because the parameter contract is generic and can have a delayed part,
`Nullable` needs to be written as a contract with a potential delayed part as
well.

Imagine we want to write a contract similar to `{_ | Bool}`, that is a
dictionary of booleans, but we also want keys to be valid numbers (although
represented as strings). A valid value could look like `{"1": true, "2": false,
"10": true}`. If we use a predicate or a validator, it's impossible to preserve
laziness: as soon as your contract is called, you would need to produce a yes or
no answer, and checking that fields are all `Bool` requires evaluating their
content first.

What we can do is to not perform all the checks right away, but **return a new
value which is wrapping the original value with delayed checks inside**:

```nickel
{
  NumberBoolDict =
    std.contract.custom
      (fun label value =>
        let with_delayed_checks =
          value
          |> std.record.map
            (fun name value =>
                let label_with_msg =
                  std.contract.label.with_message "field `%{name}` is not a boolean" label
                in
                # Note: we use `apply` and not `check` here since we
                # are inside a delayed check
                std.contract.apply Bool label_with_msg value
            )
        in

        if std.is_record value then
          value
          |> std.record.fields
          |> std.array.fold_right
            (fun field_name rest =>
              if std.string.is_match "^\\d+$" field_name then
                rest
              else
                'Error {
                  message = "field name `%{field_name}` is not a number"
                }
            )
            ('Ok with_delayed_checks)
        else
          'Error { message = "not a record" }
      )
}
```

There is a lot to unwrap here. Please refer to the [syntax section](./syntax.md)
if you have trouble parsing the example.

We first define `with_delayed_checks`, the delayed part, that maps a function
over the record which substitutes each field for the same value but wrapped in a
`Bool` contract. Thanks to lazyness, the record mapping won't happen until `'Ok
with_delayed_checks` is returned *and* some code actually uses the value. Even
then, because records (and record mapping) are lazy, *this doesn't actually
execute the `Bool` contracts right away*. Each contract will only be run when
the individual fields will be accessed or exported.

We then proceed to the immediate checks. We first check if the value is a record
and return `'Error {..}` otherwise. Then, we iterate over all the record field
names and check that each one is a sequence of digits. We use a right fold
because of its short-circuiting capabilities: as soon as an `'Error` is
encountered, `fold_right` doesn't need to evaluate the rest and returns
immediately. We provide the base value `'Ok with_delayed_checks`, which will be
picked only if all the fields are valid and the iteration goes through the end.
In that case, the immediate part has succeeded, and we return the value with
delayed checks.

We could theoretically have made the whole contact delayed by pushing the
immediate checks down each field, together with the `Bool` contract application,
but it's neither necessary nor desirable: as long as we don't force fields, it's
better to perform as many checks as possible immediately, to report errors early
and because it behaves better with respect to boolean contract combinators.

Let us see if we indeed preserved laziness:

```nickel #repl
#hide-range{1-30}

> let NumberBoolDict =
  std.contract.custom
    (fun label value =>
      let with_delayed_checks =
        value
        |> std.record.map
          (
            fun name value =>
              let label_with_msg =
                std.contract.label.with_message "field `%{name}` is not a boolean" label
              in
              std.contract.apply Bool label_with_msg value
          )
      in
      if std.is_record value then
        value
        |> std.record.fields
        |> std.array.fold_right
          (fun field_name rest =>
            if std.string.is_match "^\\d+$" field_name then
              rest
            else
              'Error {
                message = "field name `%{field_name}` is not a number"
              }
          )
          ('Ok with_delayed_checks)
      else
        'Error { message = "not a record" }
    )

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
#hide-range{1-30}

> let NumberBoolDict =
  std.contract.custom
    (fun label value =>
      let with_delayed_checks =
        value
        |> std.record.map
          (
            fun name value =>
              let label_with_msg =
                std.contract.label.with_message "field `%{name}` is not a boolean" label
              in
              std.contract.apply Bool label_with_msg value
          )
      in
      if std.is_record value then
        value
        |> std.record.fields
        |> std.array.fold_right
          (fun field_name rest =>
            if std.string.is_match "^\\d+$" field_name then
              rest
            else
              'Error {
                message = "field name `%{field_name}` is not a number"
              }
          )
          ('Ok with_delayed_checks)
      else
        'Error { message = "not a record" }
    )

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

### Conclusion

Our `NumberBoolDict` contract doesn't perform all the checks needed right away.
Instead, it performs some of them immediately and **returns a new value, which
is wrapping the original value with delayed checks inside**. Doing so preserves
laziness of the language and only triggers the checks when the values are used
or exported in a configuration. This is the reason for general custom contracts
to return an updated value as `'Ok new_value` while validators simply return
`'Ok`.

## Boolean combinators

The stdlib features a set of boolean combinators for contracts:
`std.contract.all_of`, `std.contract.any_of` and `std.contract.not`.

It's important to note that those combinators are only *approximations* of what
you could expect from boolean operations. Fortunately, they are overstrict
approximation, in that they might reject more values, but will never let invalid
values slip through.

The reason is, once again, the presence of delayed contracts.

### Combining delayed contracts

Let `Foo` be the contract `std.contract.any_of [{ foo | String }, {foo |
Number}]`. Like all built-in contracts and contract combinators, `any_of` is
designed to preserve lazyness and delayed checks, so it can't evaluate the field
`foo` to determine if it's a number or a string, as it's a delayed check of each
built-in record contract. In particular, checking the value `{foo = 1+1}`
against `Foo` will fail: `any_of` will run `{foo | String}`, whose immediate
part will succeed (the shape matches) and return `'Ok {foo | String = 1+1}`.
From there, `any_of` has no way to know that the delayed check will eventually
fail, and such delayed failures aren't catchable. Thus, the branch to pick is
based on the immediate part, and `any_of` will pick `{Foo | String}`. When `foo`
will be requested, the `String` contract will eventually be applied and will
fail.

That is, `{foo = 1+1} | Foo` will fail, which is surprising. `Foo` as defined
here is essentially useless and equivalent to just `{Foo | String}`. As both
branches have the same immediate part, they can't discriminate between values,
and either `{foo | String}` will be picked, or both contracts will fail, but the
`{foo | Number}` contract will never be picked.

Similarly, `std.contract.any_of [Array Number, Array String]` will
always behave as `Array Number`.

On the other hand, `std.contract.any_of [Number, String]` will work as expected,
because those contracts are immediate. More generally, boolean combinators work
as expected on custom contracts built from predicates or validators (using
`std.contract.from_predicate` and `std.contract.from_validator`).

Some combinations of delayed contracts, such as `std.contract.any_of [{ foo |
Number }, { bar | String }]` will also work as expected. In this case, the check
for extra fields is immediate and will be sufficient to discriminate between the
two branches immediately.

**Important** Note that the check for missing fields is *delayed* in the
built-in record contracts. When you apply a record contract, Nickel expects that
all fields might not be present *yet*, but they are rather expected to be
introduced by other parts of the configuration through merging. Instead of
raising an error right away, missing fields are just introduced as fields
without definition, which makes them delayed checks. Thus, `std.contract.any_of
[{ foo | Number, bar | String}, {foo | Number}]` won't behave as expected, and
will fail on `{foo = 1+1}`. Indeed, this combination will always pick the first
contract, setting `bar` as a field without definition to be filled later. Once
again, this combination is equivalent to its first element, the contract `{foo |
Number, bar | String}` alone. Interestingly, swapping the two contracts makes
`any_of` work as expected: `std.contract.any_of [{foo | Number}, {foo | Number,
bar | String}]`. Because `any_of` tries contracts in order, and that extra
fields *are* checked immediately, `{foo | Number}` will be able to reject a
record with a `bar` field immediately. Note that in this particular example, the
right way to write this contract is rather `{ foo | Number, bar | String |
optional }` - no need to resort to `any_of`.

### Customize the boolean behavior

The limitations mentioned above are inherent to the lazy evaluation model of
Nickel, and because we want the built-in contracts to preserve lazyness by
default.

However, if you hit this limitation, you can always decide to build a custom
contract that will be able to discriminate between more values immediately, at
the cost of evaluating more data.

For example, consider an encoding of an enum as a record with a tag and a value:
accepted values are records of the shape `{tag, value}` where the value is a
number if `tag` is `'Number`, or a string if `tag` is `'String`.

A naive implementation is `std.contract.any_of [{ tag = 'String, value | String
}, { tag = 'Number, value | Number }]`. This has the exact problem described in
the previous section: this is equivalent to the contract `{ tag | 'String, value
| String }`, and will reject the seemingly legal value `{ tag = 'Number, value =
1+1 }`.

The trick is to use a custom contract that performs more immediate checks. There
are many different ways to approach this problem, depending on the specifics of
each situation. Here, we'll write a little `Tagged` contract wrapper that takes
a record contract, expects `tag` to exist and to be set to a fixed value, and
will check if the provided value has a matching `tag` immediately.

```nickel #repl
> let Tagged = fun Contract =>
  std.contract.custom (fun label =>
    match {
      value @ { tag, .. } if tag == Contract.tag =>
        std.contract.check Contract label value,
      { tag, .. } =>
        'Error { message = "incompatible tag field" },
      _ =>
        'Error { message = "missing tag field" },
    }
  )

> let NumberOrString = std.contract.any_of [
    Tagged { tag = 'String, value | String },
    Tagged { tag = 'Number, value | Number },
  ]

> { tag = 'Number, value = 1+1 } | NumberOrString
{ tag = '"Number", value | Number = 2, }

> { tag = 'String, value = "hello"} | NumberOrString
{ tag = '"String", value | String = "hello", }

> { tag = 'Number, value = "hello"} | NumberOrString
error: contract broken by the value of `value`
[...]
```

### Immediate version of built-in contracts

In the future, we plan to provide alternative versions of built-in contracts,
like `{ foo | Number, bar | String }` or `Array {_ | Number}`, but which are
fully immediate with no delayed checks. This would allow to use them in boolean
combinators with the intuitive semantics, at the price of needing to deeply
evaluate all the content immediately. This might be useful when converting
schemas from different tools to Nickel blindly without having to write a lot of
custom contract boilerplate.

As of today, those immediate versions are not yet available. It is advised to
rely on custom wrappers in the meantime, as explained in the previous section.

## Other combinators

`any_of` has been used as a leading example, but `not` has the same limitations.
For example, `["a"] | std.contract.not (Array Number)` will fail, as
`std.contract.not (Array Number)` is essentially the same as `std.contract.not
(Array Dyn)`, and will reject any array.

On the other hand, `all_of` doesn't suffer from the same limitations, since it
has to apply all contracts unconditionally.

One exception worth noting is function contracts, which are mostly delayed, and
might be overstrict with all boolean operators including `all_of`: `all_of
[Number -> Number, String -> String]` will reject all values, including the
identity function `fun x => x`, because it's the same contract as
`std.contract.all_of [Number, String] -> std.contract.all_of[Number, String]`,
but `std.contract.all_of [Number, String]` is void. Because `fun x => x` can be
annotated with the `Number -> Number` and `String -> String` contracts
individually, one could expect that the `any_of` combination would work as well,
but it doesn't.
