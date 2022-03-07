---
slug: contracts
---

# Contracts in Nickel

(For the motivation behind contracts and a high-level overview of contracts and
types, first read the [correctness](./correctness.md) section.)

To first approximation, contracts are assertions. They check that a value
satisfies some property at run-time. If the test passes, the execution can go on
normally. Otherwise, an error is raised. In Nickel, you can apply a contract
using the `|` operator:

```nickel
let x = (1 + 1 | Num) in x
```

Contract can also be attached to identifiers in a definition:

```nickel
# let-binding: equivalent to the previous example
let x | Num = 1 + 1 in x

# on a record field
{x | Num = 1 + 1}
```

Here, `x` is bound to a `Num` contract. When evaluating `x`, the following steps
are performed:

1. Evaluate `1 + 1`.
2. Check that the result is a number.
3. If it is, return the expression unchanged. Otherwise, raise an error.

```
$ nickel repl
nickel>1 + 1 | Num
2

nickel>"a" | Num
error: contract broken by a value.
[..]
```

Contracts corresponding to the basic types `Num`, `Str`, `Bool` and `Dyn` are
available. `Dyn` is a contract that never fails.

## User-defined contracts

### By hand

Where run-time
mechanisms like contracts really shine is in the ability to check for arbitrary
properties. Let us see how to define our very own contract. We start the REPL
(`nickel repl`) and input:

```nickel
let IsFoo = fun label value =>
  if builtin.is_str value then
    if value == "foo" then
      value
    else
      contract.blame_with "not equal to \"foo\"" label
  else
    contract.blame_with "not a string" label
```

A custom contract is a function of two arguments:

- A `label`. Provided by the interpreter, the label contains tracking information
  for error reporting. Its main usage is to be passed to `contract.blame` or
  `contract.blame_with` when the contract isn't satisfied.
- The value being checked.

Upon success, the contract must return the original value. We will see the reason why
in the [laziness](#laziness) section. To signal failure, we use
`contract.blame` or its variant `contract.blame_with` that takes an additional
error message as a parameter. `blame` immediately aborts the execution and
reports a contract violation error.

In `IsFoo`, we first test if the value is a string, and then if it is equal to
`"foo"`, in which case the contract succeeds. Otherwise, the contract fails with
appropriate error messages. Let us try:

```
nickel>1 | IsFoo
error: contract broken by a value [not a string].
[..]

nickel>"a" | IsFoo
error: contract broken by a value [not equal to "foo"].
[..]

nickel>"foo" | IsFoo
"foo"
```

### With `from_predicate`

Our contract is simple: in the end, it tests the condition `value == "foo"`.
Unfortunately, it has a few cascading ifs that don't look very nice. This is a
necessary evil if you want to provide distinct error messages (`not a string`
and `not a "foo"`). However, one could argue in this case that the information
of the contract's name (which is automatically printed for contract violiation
error, as well as various other data) is enough to understand the error. In this
case, we can write our contract more succinctly as:

```nickel
let IsFoo = contract.from_predicate ((==) "foo")
```

`contract.from_predicate` takes a predicate (a function of one argument that
returns a boolean) and converts it to a contract. The syntax `(==)` turns the
equality operator `==` into a function, and is a shorthand for
`fun x y => x == y`. The partial application to `(==) "foo"` is then the function
`fun y => "foo" == y`, which is exactly the condition we want. `from_predicate`
is useful to quickly define contracts based on a boolean condition, and when
the contract is simple enough to not require custom error message.

Here is an example of a port number contract:

```nickel
let Port = contract.from_predicate (fun value =>
  builtin.is_num value &&
  num.is_int value &&
  value >= 0 &&
  value <= 65535)
```

### Parametrized contracts

Let us consider a contract for bound checking:

```nickel
let Between5And10 = contract.from_predicate (fun value =>
  builtin.is_num value &&
  value >= 5 &&
  value <= 10) in
let MyConfig = {
  level | Between5And10,
}
```

Now, we add a new field to our schema, that must be between `0` and `1`:

```nickel
let Between5And10 = contract.from_predicate (fun value =>
  builtin.is_num value &&
  value >= 5 &&
  value <= 10) in
let Between0And1 = contract.from_predicate (fun value =>
  builtin.is_num value &&
  value >= 0 &&
  value <= 1) in
let MyConfig = {
  level | Between5And10,
  strength | Between0And1,
}
```

`Between5And9` and `Between0And1` are awkwardly similar. Because custom
contracts are nothing more than functions, we can use function arguments as
parameters for contracts. Given how partial function application works,
parameters must appear first, before the `label` and `value` arguments:

```nickel
let Between = fun min max =>
  contract.from_predicate (fun value =>
    value >= min &&
    value <= max) in
# alternative without from_predicate
let BetweenAlt = fun min max label value =>
  if builtin.is_num value &&
     value >= min &&
     value <= max then
    value
  else
    contract.blame label in
let MyConfig = {
  level | Between 5 10,
  strength | Between 0 1,
}
```

### Contracts parametrized by contracts

Contracts parametrized by other contracts are not really special among
parametrized contracts, but note that although contracts can be functions, we
will see soon that they can be different objects as well. Even with functions,
contract application behaves slightly differently than bare function
application. Thus, when manually handling a contract `contract`, do not apply it
as a function `contract label value`, but use `contract.apply`:
`contract.apply contract label value`. One example of such a contract is a
`Nullable` contract, that accepts a value that is either null or of some other
given format:

```nickel
let Nullable = fun contract label value =>
  if value == null then
    value
  else
    contract.apply contract label value in
# succeeds
null | Nullable Num
# succeeds too
1 | Nullable Num
```

## Compound contracts

This section describes the different ways of composing existing contracts
together to create new ones.

### Records

Record contracts are fundamental in a configuration language like Nickel. They
encode the data schema of a configuration. A record contract is just a record
literal with contract annotations and whose field definitions are usually
missing:

```nickel
let MyConfig = {
  path | Str,

  connection | {
    server_port | Port,
    host | Str,
  }
} in

{
  path = "/foo/bar",
  connection = {
    server_port =
      if host == "localhost" then
        8080
      else
        80,
    host = "localhost",
  }
} | MyConfig
```

`MyConfig` is a schema describing a configuration with:

- a field `path` that must be a string
- a sub-record `connection`, which has a `server_port` and a `host` field that
  are respectively a port number and a string.

See how we've combined contracts: while defining the `MyConfig` record contract,
we wrote another inline record contract for the `connection` field, and we
reused the `Port` contract defined earlier. Composition is this ability of using
existing contracts seamlessly as building blocks for new contracts.

If we export this example to json, we get:

```
$ nickel -f config.ncl export
error: contract broken by a value.
   ┌─ :1:1
   │
 1 │ Port
   │ ---- expected type
   │
   ┌─ .../config.ncl:22:9
   │
22 │         "8080"
   │         ^^^^^^ applied to this expression
   │
   ┌─ <unknown> (generated by evaluation):1:1
   │
 1 │ "8080"
   │ ------ evaluated to this value

note:
   ┌─ .../config.ncl:12:19
   │
12 │     server_port | Port,
   │                   ^^^^ bound here
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
strings, you can amend the `Port` contract to be more liberal and accept strings
representing valid port numbers.

#### Metadata

Additionally to defining the contracts of fields, record contract can also
attach metadata (see [merging](./merging.md)) to fields, such as a documentation
or default value:

```
nickel>let MyConfig = {
    foo | doc "This documentation will propagate to the final value!"
        | Str
        | default = "foo",
    bar | Num,
}
nickel>let config | MyConfig = {bar = 2}
nickel> builtin.serialize `Json config
"{
  "bar": 2,
  "foo": "foo"
}"
nickel>
nickel>:query config.foo
* contract: Str
* default: "foo"
* documentation: This documentation will propagate to the final value!
```

#### Open record contracts

By default, record contracts are closed, meaning that additional fields are forbidden:

```
nickel>let Contract = {foo | Str}
nickel>{foo = "a", bar = 1} | Contract
error: contract broken by a value [extra field `bar`].
[..]
```

If you want to allow additional fields, append `, ..` after the last field
definition to define an open contract:

```
nickel>let Contract = {foo | Str, ..}
nickel>{foo = "a", bar = 1} | Contract
{ foo = <contract,value="a">, bar = 1}
```

#### Giving values for fields

While most record contracts don't have field definitions, they can. In fact,
records contracts are not special: any record literal can be used both as a
contract or a normal value. If a field is defined both in the contract and the
checked value, the two definitions will be merged in the final result. For
example:

```
nickel>let Secure = {
  must_be_very_secure | Bool = true,
  data | Str,
}
nickel>builtin.serialize `Json ({data = ""} | Secure)
"{
  "data": "",
  "must_be_very_secure": true
}"

nickel>{data = "", must_be_very_secure = false} | Secure
error: Non mergeable terms
  ┌─ repl-input-7:1:35
  │
1 │ {data = "", must_be_very_secure = false} | Secure
  │                                   ^^^^^ cannot merge this expression
  │
  ┌─ repl-input-4:2:32
  │
2 │   must_be_very_secure | Bool = true,
  │                                ^^^^ with this expression
```

**Warning: `=` vs `|`**

It may be tempting to use `=` instead of  `|` to attach a record contract to a
field. That is, writing `Contract = {foo = {bar | Str}}` instead of `Contract =
{foo | {bar | Str}}`. When applying this contract, the merging operator will
apply the `Str` contract to the field `foo` of the checked value. At first
sight, `=` also fits the bill. However, there are a number of subtle but
potentially surprising differences.

One concerns open contracts. Merging never requires the presence of specific
fields: thus, the contract `{bar | Str}` attached to `foo` will actually behave
as an open contract, even if you didn't use `..`. This is usually not what you
want:

```
nickel>let ContractPipe = {
  sub_field | {foo | Str}
}
nickel>let ContractEq = {
  sub_field = {foo | Str}
}
nickel>{sub_field.foo = "a", sub_field.bar = "b"} | ContractPipe
error: contract broken by a value [extra field `bar`].
[..]

nickel>{sub_field.foo = "a", sub_field.bar = "b"} | ContractEq
{ sub_field = { foo = <contract,value="a">, bar = "b"}}
```

There are other discrepancies, e.g. when applying the contract to a record with
a `default` annotation on `subfield`. Thus, unless you have a specific use-case
in mind, **you should use `|` instead of `=` when attaching record contracts**.

### Types constructors for contracts

We've already seen that the primitive types `Num`, `Str` and `Bool` can be used
as contracts. In fact, any type constructor of the
[static type system](./typing.md) can be used to combine contracts.

#### Array

An array contract checks that the value is an array and applies the parameter
contract to each element:

```
nickel>let VeryBig = contract.from_predicate (fun value =>
  builtin.is_num value
  && value >= 1000)
nickel>[1000, 10001, 2] | Array VeryBig
error: contract broken by a value.
  ┌─ :1:7
  │
1 │ Array (VeryBig)
  │        ------- expected array element type
  │
  ┌─ repl-input-14:1:15
  │
1 │ [1000, 10001, 2] | Array VeryBig
  │               - evaluated to this expression
[..]
```

#### Functions

A function contract `In -> Out` returns a wrapped function which, for each call,
will additionally check the parameter against the `In` contract and the return
value against the `Out` contract. Put differently, `In` represents
*pre-conditions* that must hold about the parameter, and `Out` *post-conditions*
that must hold about the return value of the function.

##### Caller vs callee

Function contracts, as opposed to a contract like `Num`, have the peculiarity of
involving two parties in the contract checking. For example:

```nickel
let add_semi | Str -> Str = fun x => x ++ ";" in
add_semi 1

let wrong | Str -> Str = fun x => 0 in
wrong "a"
```

Both of those examples will fail with a contract violation. But they are
different in nature: in the first one, the function `add_semi` respects its
contract. Whenever `x` is a string, `add_semi` does return a string. Here, the
**caller** (the user of the function) is to blame, and not the **callee** (the
function). This is an important distinction. Say you wrote `add_semi` as a part
of a library. A wrong call is a random user, somewhere, making a programming
mistake. This is not your direct responsibility, assuming the contract error
messages are good enough to let users understand the issue immediately.

The second example is reversed. The caller provides a string, as requested by
the contract, but the function returns a number. The blame is on the **callee**.
If you are the library writer who shipped the `wrong` function, this means you
have a bug that you need to fix.

The interpreter automatically performs book-keeping for functions contracts in
order to make this caller/callee distinction:

```
nickel>let add_semi | Str -> Str = fun x => x ++ ";" in
add_semi 1
error: contract broken by the caller.
[..]

nickel>let wrong | Str -> Str = fun x => 0 in
wrong "a"
error: contract broken by a function.
[..]
```

##### Higher-order functions

The beauty of function contracts is that they gracefully scale to higher-order
functions as well. Higher-order functions are functions that take other
functions as parameters. Here is an example:

```
nickel>let apply_fun | (Num -> Num) -> Num = fun f => f 0 in
apply_fun (fun x => "a")
error: contract broken by the caller.
  ┌─ :1:9
  │
1 │ (Num -> Num) -> Num
  │         --- expected return type of a function provided by the caller
  │
  ┌─ repl-input-17:2:21
  │
2 │ apply_fun (fun x => "a")
  │                     --- evaluated to this expression
[..]
```

#### Dictionary

The type constructor `{_ : Contract}` represents a record whose field names are not
constrained but whose field values are checked against `Contract`. In practice, the
contract `Contract` is mapped onto each field. Such a contract is useful when using
records as an extensible dictionary, that is a key/value store, where keys are
strings and values respect `Contract`. Example:

```nickel
let occurrences | {_: Num} = {a = 2, b = 3, "!" = 5, "^" = 1} in
occurrences."!"
```

## Laziness

In the [writing a custom contract by hand](#by-hand) section, we noted the
strange fact that a custom contract must return a value, instead of just
returning e.g. a boolean to indicate success or failure. A contract could even
simply return `null`, as failure is handled separately by aborting. Moreover, the
contracts we have written so far always returned the original unmodified
value upon success, which doesn't sound very useful: after all, the caller and
the interpreter already had access to this value to begin with.

The motivation for this return value relates to laziness. Nickel is designed to
be *lazy*, meaning that values are evaluated on-demand, only when they are
needed. For example, record fields are not evaluated until they are accessed,
printed (in the REPL or as a result of `nickel`), or until the whole record is
serialized/exported. Thanks to laziness, you can for example query specific
information on a large configuration without having to actually evaluate
everything. We will use a dummy failing expression `1 + "a"` to observe where
evaluation takes place:

```
nickel>let config = {fail = 1 + "a", data | doc "Some information" = 42}
nickel>:query config.data
* value: 42
* documentation: Some information

nickel>config.fail
error: type error
[..]
```

See how the command `:query config.data` succeeds, although the field `fail`
causes an error when evaluated.

A large configuration will most probably have a root contract attached,
corresponding to its schema. If this contract checked everything eagerly,
forcing the evaluation of the most part of the configuration, we would lose the
benefits of laziness. Thus, *we want contracts to be lazy as well*. In
particular, a subcontract attached to a field should only fire when the field is
requested. This is the case with record contracts, and in general all native
contracts combinators are lazy too.

### Writing lazy contracts

Imagine we want to write a contract similar to `{_ : Bool}`, that is a
dictionary of booleans, but we also want keys to be number literals (although
represented as strings). A valid value could look like `{"1": true, "2": false,
"10": true}`. If we used boolean predicates as the default for contracts, it
would be impossible to make it lazy: as soon as your contract is called, you
would need to produce a `true` or `false` answer, and checking that fields are
all `Bool` would force their evaluation.

What we can do is to not perform all the checks right away, but **return a new
value, which is wrapping the original value with delayed checks inside**. This
is the rationale behind contracts returning a value. Let us see:

```nickel
let NumBoolDict = fun label value =>
  if builtin.is_record value then
    let check_fields = value
      |> record.fields
      |> array.foldl (fun acc field_name =>
        if string.is_match "^\\d+$" field_name then
          acc # unused and always null through iteration
        else
          contract.blame_with "field name `#{field_name}` is not a number" label
        ) null in

    value
      |> record.map (fun name value =>
        let label_with_msg =
            contract.tag "field `#{name}` is not a boolean" label in
        contract.apply Bool label_with_msg value)
      |> builtin.seq check_fields
  else
    contract.blame_with "not a record" label
```

There is a lot to unwrap here. Please refer to the [syntax](./syntax.md) section
if you have trouble parsing the example. We first check that the value is a
record on line 2. We then define `check_fields` one line 3, an expression that
goes over the record field names and check that each one is a sequence of
digits. We use a left fold with a dummy `null` accumulator as a way to just
iterate through the array without building up anything interesting.

For laziness, the interesting bit happens here:

```nickel
value
  |> record.map (fun _name value =>
    contract.apply Bool label value)
  |> builtin.seq check_fields
```

This is the final return value of the contract (at least when `value` is a
record). This code takes the original record, and maps a function which
substitutes each fields for the same value but wrapped in a `Bool` contract on
it. Because records are lazy, *this doesn't actually execute the `Bool`
contracts right away*. They will be run only when the corresponding field will
be requested.

Finally, we sequence the result with `check_fields`: if we remove this last line
and return the mapped value directly, because Nickel is lazy and `check_fields`
is not used elsewhere, the check wouldn't actually trigger. Using `seq` first
forces the evaluation of `check_fields` unconditionally, ignores the resulting
value and continues with the second argument (here, our wrapped `value`).

Let us see if we indeed preserved laziness:

```
nickel>let config | NumBoolDict = {
    "1" = 1 + "a", # Same as our previous "fail"
    "0" | doc "Some information" = true,
}
nickel>:query config."0"
* value: true
* documentation: Some information
```

Yes! Our contract doesn't unduly cause the evaluation of the field `"1"`. Does
it check anything, though?

```
nickel>let config | NumBoolDict = {
  not_a_number = false,
  "0" | doc "Some information" = false,
}
nickel>:q config."0"
error: contract broken by a value [field name `not_a_number` is not a number].
[..]

nickel>let config | NumBoolDict = {
  "0" | doc "Some information" = "not a boolean",
}
nickel>:q config."0"
error: contract broken by a value [field `0` is not a boolean].
[..]
```

It does!

#### Remark: lazy, always?

Note that our check for field names is not lazy in the sense that even when
requesting another field `"0"`, we've already triggered this check for all field
names. This is totally fine: early error reporting is a good thing, as long we
don't force the evaluation of something that wouldn't be evaluated without the
contract. The field names of a record are readily available and don't incur any
evaluation, so we can check for them directly. We could also have put this check
inside each field, along the `Bool` contract application, making it lazy. But we
prefer to report this error as soon as possible.

#### Conclusion

Our `NumToBool` contract doesn't perform all the checks needed right away.
Instead, **it returns a new value, which is wrapping the original value with
delayed checks inside**. Doing so preserves laziness of the language and only
triggers the checks when the values are used or exported in a configuration.
This is the reason for contracts to return a value, which must be the original
value with potential delayed checks inside.
