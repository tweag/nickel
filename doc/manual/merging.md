---
slug: merging
---

# Merging records

In Nickel, the basic building blocks for data are records (objects in JSON or
attribute sets in Nix). Merging is a fundamental built-in operation whose role
is to combine records together. Fields common to several records will be
themselves recursively merged if possible, following the semantics described in
this document.

Merging is useful to compose small and logical blocks into a potentially complex
final configuration, making it more manageable. A merge is performed by the `&`
operator.

Merge is a **symmetric** operation. More pedantically, it is commutative. In
practice, this means that order doesn't matter, and `left & right` is the same
thing as `right & left`. When the operands need to be distinguished, as we
will see for default values for example, the idea is to use special annotations
(metadata) instead of relying on the order of the operands.

**Warning**: At the time of writing, Nickel's version is 1.0. Custom merge
functions are planned for a future version. They are not detailed here yet. See
the associated technical document [RFC001][rfc001].

The section describes the behavior and use-cases of merge, by considering the
following situations:

<!-- markdownlint-disable MD051 -->

- [Merging records without common fields](#simple-merge-no-common-fields)
- [Merging records with common fields](#recursive-merge-with-common-fields)
- [Merging records with metadata](#merging-records-with-metadata)
  - [Optional fields](#optional-fields)
  - [Default values](#default-values)
  - [Contracts](#contracts)
  - [Documentation](#documentation)
  - [Not exported](#not-exported)
- [Recursive overriding](#recursive-overriding)

<!-- markdownlint-enable MD051 -->

## Simple merge (no common fields)

Merging two records with no common fields results in a record with the fields
from both operands. For example, `{foo = 1, bar = "bar"} & {baz =false}`
evaluates to `{foo = 1, bar = "bar", baz = false}`.

### Specification

Formally, if we write the left operand as

```nickel #no-check
left = {
  field_left_1 = value_left_1,
  ..,
  field_left_n = value_left_n,
}
```

and the right operand as

```nickel #no-check
right = {
  field_right_1 = value_right_1,
  ..,
  field_right_k = value_right_k
}
```

then the merge `left & right` evaluates to the record:

```nickel #no-check
{
  field_left_1 = value_left_1,
  ..,
  field_left_n = value_left_n,
  field_right_1 = value_right_1,
  ..,
  field_right_k = value_right_k
}
```

In other words, `left & right` is the union of `left` and `right`.

### Examples

#### Split

You can split a configuration into subdomains:

```nickel #no-check
# file: server.ncl
{
    host_name = "example",
    host = "example.org",
    ip_addr = "0.0.0.0",
}

# file: firewall.ncl
{
    enable_firewall = true,
    open_ports = [23, 80, 443],
}

# file: network.ncl
let server = import "server.ncl" in
let firewall = import "firewall.ncl" in
server & firewall
```

This gives:

```nickel
{
    host_name = "example",
    host = "example.org",
    ip_addr = "0.0.0.0",
    enable_firewall = true,
    open_ports = [23, 80, 443],
}
```

#### Extension

Given a configuration, you can use merge to add new fields:

```nickel #no-check
# file: safe-network.ncl
let base = import "network.ncl" in
base & {use_iptables = true}
```

## Recursive merge (with common fields)

When the two operands have fields in common, those fields are recursively
merged. For example:

```nickel
{
  top_left = 1,
  common = {left = "left"}}
& {
  top_right = 2,
  common = {right = "right"}
}
```

Evaluates to the record:

```nickel
{
  top_left = 1,
  top_right = 2,
  common = {left = "left", right = "right"}
}
```

When one or both of the common fields are not records, the merge will fail
unless one of the following condition hold:

- They are both of the same primitive data type (`Number`, `Bool`, `EnumTag`, or
  `String`) and they are equal
- They are both enum variants, and their tags are equal. In this case, the
  arguments are merged recursively: that is,
  `'Tag arg1 & 'Tag arg2` is `'Tag (arg1 & arg2)`.
- They are both arrays, and they are equal (checked by generating an application
  of the lazy contract `std.contract.Equal`)
- They are both equal to `null`

### Specification

Formally, let's write the left operand as

```nickel #no-check
left = {
  field_left_1 = value_left_1,
  ..,
  field_left_n = value_left_n,
  common_1 = common_vleft_1,
  ..,
  common_m = common_vleft_m,
}
```

and the right operand as

```nickel #no-check
right = {
  field_right_1 = value_right_1,
  ..,
  field_right_k = value_right_k
  common_1 = common_vright_1,
  ..,
  common_m = common_vright_m,
}
```

where the `field_left_i` and `field_right_j` are distinct for all `i` and `j`.
Then the merge `left & right` evaluates to the record:

```nickel #no-check
{
  field_left_1 = value_left_1,
  ..,
  field_left_n = value_left_n,
  field_right_1 = value_right_1,
  ..,
  field_right_k = value_right_k
  common_1 = common_vleft_1 & common_vright_1,
  ..,
  common_m = common_vleft_m & common_vright_m,
}
```

For two values `v1` and `v2`, if at least one value is not a record, then

```text
v1 & v2 = v1               if (type_of(v1) is Number, Bool, String, Enum or
                                v1 == null)
                              AND v1 == v2

          v2 | Equal v1    if type_of(v1) is Array and type_of(v2) is Array

          _|_              otherwise (indicates failure)
```

### Example

```nickel #no-check
# file: udp.ncl
{
    # same as firewall = {open_ports = {udp = [...]}},
    firewall.open_ports.udp = [12345,12346],
}

# file: tcp.ncl
{
    # same as firewall = {open_ports = {tcp = [...]}},
    firewall.open_ports.tcp = [23, 80, 443],
}

# firewall.ncl
let udp = import "udp.ncl" in
let tcp = import "tcp.ncl" in
udp & tcp
```

In the above example, we merge two records, both with a field `firewall`. On
both sides, the value of `firewall` is again a record, which is therefore
merged. The same process happens one layer below, on the common field
`open_ports`, to result in the final record:

```nickel
{
    firewall = {
      open_ports = {
        udp = [12345, 12346],
        tcp = [23, 80, 443],
      }
    }
}
```

## Merging records with metadata

Metadata can be attached to values thanks to the `|` operator. Metadata
currently include contract annotation, default value, merge priority,
optionality annotation, and documentation. We describe in this section how
metadata interact with merging.

Note that metadata can only be syntactically attached to record fields, except
for type and contract annotations, which can appear anywhere in a freestanding
expression such as `(x | Num) + 1`. However, a contract annotation outside a
record field isn't considered metadata (it's a mere contract check) and doesn't
behave the same with respect to merging. **In particular, `{foo | Num = 1}` can
behave differently from `{foo = (1 | Num)}` when merged**. See [the contracts
section](#contracts) for more details.

### Optional fields

A field can be marked as optional using the `optional` annotation:

```nickel
# schema.ncl
{
  Command = {
    command
      | String,
    arg_type
      | [| 'String, 'Number |],
    alias
      | String
      | optional,
  },
}
```

Optional fields are intended to be used in record contracts to mean that a field
might be defined, but is not mandatory. An optional field initially doesn't have
a definition [^optional-with-value]. A value is provided by applying the
contract to - or merging it with - a record which defines a value for this
field:

```nickel
# hide-start
let Command = {
    command
      | String,
    arg_type
      | [| 'String, 'Number |],
    alias
      | String
      | optional,
  } in
# hide-end
{
  command = "exit",
  arg_type = 'String,
  alias = "e",
} | Command
```

Once an optional field becomes defined, it acts just like a regular field. Any
attached contract will be applied as well (here, `String` on `alias`). An
optional field also becomes a regular field as soon as it is merged with the
same field that doesn't have the `optional` annotation. This holds **even if the
other field doesn't have a definition**:

```nickel #repl
> {foo = 1, bar | optional} & {bar | optional}
{ foo = 1, }

> {foo = 1, bar | optional} & {bar}
error: missing definition for `bar`
  ┌─ <repl-input-1>:1:12
  │
1 │  {foo = 1, bar | optional} & {bar}
  │  ----------^^^--------------------
  │  │         │
  │  │         required here
  │  in this record
```

In the second example, `bar` isn't optional on the right-hand side of the merge.
Although the right-hand side `bar` doesn't have a definition, the resulting
`bar` field isn't optional anymore.

As long as an optional field doesn't have a value, it will be invisible to
record operations. Optional fields without a value don't show up in
`std.record.fields`, they won't make `std.record.values` throw a missing field
definition error, etc.

```nickel #repl
> let Contract = {foo = 1, bar | optional}

> std.record.values Contract
[ 1 ]

> std.record.has_field "bar" Contract
false
```

Optional fields can still be discovered through metadata queries (run `nickel
help query` or type `:help query` in the REPL for more information) or generated
documentation.

[^optional-with-value]: You can actually provide a value (default or not) for an optional
  field, but this nullifies the effect of `optional`: what you get is just a
  regular field

### Merge priorities

Priorities are specified using the `priority` annotation, followed by a number
literal. There are also two other special priority annotations, the lowest
priority `default`, and the highest priority `force`. Both are written without
the `priority` keyword.

Priorities dictate which values take precedence over other values. By default,
values are given the priority `0`. Values with the same priority are recursively
merged as specified in this document, which can mean failure if the values can't
be meaningfully combined:

```nickel #repl
> {foo = 1} & {foo = 2}
error: non mergeable terms
  ┌─ <repl-input-5>:1:9
  │
1 │  {foo = 1} & {foo = 2}
  │  -------^-----------^-
  │  │      │           │
  │  │      │           with this expression
  │  │      cannot merge this expression
  │  originally merged here
  │
  = Merge operands have the same merge priority but they can't be combined.
  = Both values are of type Number but they aren't equal.
  = Number values can only be merged if they are equal
```

If the priorities differ, the value with the highest priority simply erases the
other:

```nickel #repl
> {foo | priority 1 = 1} & {foo = 2}
{ foo | priority 1 = 1, }

> {foo | priority -1 = 1} & {foo = 2}
{ foo = 2, }
```

The priorities are ordered in the following way:

- `default` is the lowest priority
- numeral priorities are ordered as usual numbers (priorities can be any valid Nickel
  number, including fractions and negative values)
- `force` is the highest priority

#### Default values

A `default` annotation can be used to provide a base value, but have it be
overridable through merging. For example, `{foo | default = 1} & {foo = 2}`
evaluates to `{foo = 2}`. A default value is just a special case of a priority:
the lowest possible one.

#### Forcing values

Dually, values with the `force` annotation are given the highest priority. Such
a value can never be overridden, and will either take precedence over another
value or be tentatively merged if the other value has priority `force` as well.

#### Specification

Each field definition `foo = val` is assigned a priority `p(val)`. When merging
two common fields `value_left` and `value_right`, the result is either the one
with the highest priority (which overrides the other), or the two are
tentatively recursively merged if the priorities are equal. Without loss of
generality, consider the simple case of two records with only one common
field:

```text
{common = left} & {common = right}
= {
  common = left          if p(left) > p(right)
           right         if p(left) < p(right)
           left & right  if p(left) = p(right)
}
```

#### Example

Let us stick to our firewall example. Thanks to default values, we can set the most
restrictive configuration by default, but have it still be overridden if needed.

Let us first try without default values:

```nickel
let base = {
  firewall.enabled = true,
  firewall.type = "iptables",
  firewall.open_ports = [21, 80, 443],
} in
let patch = {
  firewall.enabled = false,
  server.host.options = "TLS",
} in
base & patch
```

Because merging is meant to be symmetric, Nickel is unable to know which value
to pick between `enabled = true` and `enabled = false` for the firewall, and
thus it will fail:

```text
error: non mergeable terms
   ┌─ repl-input-8:2:22
   │
 2 │   firewall.enabled = true,
   │                      ^^^^ cannot merge this expression
   ·
 7 │   firewall.enabled = false,
   │                      ^^^^^ with this expression
   ·
10 │ base & patch
   │ ------------ originally merged here
   │
   = Merge operands have the same merge priority but they can't be combined.
   = Both values are of type Bool but they aren't equal.
   = Bool values can only be merged if they are equal
```

We can use default values to give the priority to the right side:

```nickel
let base = {
  firewall.enabled | default = true,
  firewall.type | default = "iptables",
  firewall.open_ports | default = [21, 80, 443],
} in
let patch = {
  firewall.enabled = false,
  server.host.options = "TLS",
} in
base & patch
```

This evaluates to:

```nickel
{
  firewall = {
    enabled = false,
    open_ports = [21, 80, 443],
    type = "iptables",
  },
  server = {
    host = {
      "options" = "TLS",
    }
  }
}
```

### Contracts

*Note*: see the [correctness section](./correctness.md) and the
[contracts section](./contracts.md) for a thorough introduction to contracts in
Nickel.

Fields may have contracts attached, either directly, as in `{foo | Number = 1}`,
or propagated from an annotation higher up, as in `{foo = 1} | {foo | Number}`.
In both cases, `foo` must satisfy the contract `Number`. What happens if the
value of `foo` is altered in a subsequent merge? For example:

- Should `{foo | default | Number = 1} & {foo = "bar"}` succeed, although `foo`
  would be a string in the final result?
- Should
  `{foo | {subfield | String} = {subfield = "a"}} & {foo.other_subfield = 1}`
  succeed, although a closed contract `{subfield | String}` is attached to
  `foo`, and the final result would have an additional field `other_subfield` ?

Nickel chooses to answer **no** to both. In general, when a contract is attached
to a field `foo`, merging ensures that whatever this field is merged with,
including being dropped in favor of another value, the final value for `foo` has
to respect the contract as well or the evaluation will fail accordingly.

This is only true for *contracts attached directly to record fields* (either
directly, or coming from an enclosing record contract). In particular,
`{foo | Number = 1} & {foo | force = "bar"}` will fail, but
`{foo = (1 | Number)} & {foo | force = "bar"}` will succeed. In the latter case,
the contract is not considered to be field metadata, but a local contract check,
which is not propagated by merging.

#### Specification

Consider two operands with one field each, which is the same on both side, and
respective contracts `Left1, .., Leftn` and `Right1, .., Rightk` attached

```nickel #no-check
left = {
  common | Left1
         | ..
         | Leftn
}
```

and

```nickel #no-check
right = {
  common | Right1
         | ..
         | Rightk
}
```

Then the `common` field of `left & right` will be checked against `Left1, ..,
Leftn, Right1, .., Rightk`.

The accumulated contracts are applied lazily: as long as the field's value isn't
requested, contracts are accumulated but not yet applied. This makes it possible
to build a value piecewise, whereas the intermediate values don't necessarily
satisfy the contract. For example:

```nickel
let FooContract = {
  required_field1,
  required_field2,
} in
{ foo | FooContract}
& { foo.required_field1 = "here" }
& { foo.required_field2 = "here" }
```

Running the above program succeeds, even though the intermediate value
`{foo.required_field1 = "here"}` doesn't respect `FooContract` (it misses the
field `required_field2`).

If we try to observe the intermediate result (`deep_seq` recursively forces the
evaluation of its first argument and proceeds with evaluating the second
argument), we do get a contract violation error:

```nickel #repl
> let FooContract = {
    required_field1,
    required_field2,
  }
  in
  let intermediate =
    { foo | FooContract }
    & { foo.required_field1 = "here" }
  in
  intermediate
  & { foo.required_field2 = "here" }
  |> std.deep_seq intermediate
error: missing definition for `required_field2`
     ┌─ <repl-input-8>:3:5
     │
   3 │     required_field2,
     │     ^^^^^^^^^^^^^^^ required here
     ·
   8 │     & { foo.required_field1 = "here" }
     │             ------------------------ in this record
     │
[...]
```

#### Example

We might want to require that only non-privileged port numbers are used in a
configuration. This could for example be done as follows:

```nickel
let Port
  | doc "A valid port number"
  =
    std.contract.from_predicate
      (
        fun value =>
          std.is_number value
          && value % 1 == 0
          && value >= 0
          && value <= 65535
      )
  in

let GreaterThan
  | doc "A number greater than the parameter"
  = fun x => std.contract.from_predicate (fun value => value > x)
  in

{
  port
    | GreaterThan 1024
    | default
    = 8080,
}
& {
  port | Port = 80,
}
```

Because 80 would be less than 1024, this fails at evaluation:

```text
error: contract broken by the value of `port`
   ┌─ example.ncl:27:17
   │
22 │     | GreaterThan 1024
   │       ---------------- expected type
   ·
27 │   port | Port = 80,
   │                 ^^ applied to this expression
```

### Not exported

A field can be marked as not exported, using the `not_exported` annotation.
Such a field will behave like a regular field during evaluation, but it will
be ignored during serialization: such a field won't appear in the output of
`nickel export` nor in the output of `std.serialize`. It won't be evaluated upon
serialization either.

For example, say we want to add some high-level configuration field to a modular
configuration, from which other fields are derived:

```nickel
# hello-service.ncl
{
  greeter
    | String
    | not_exported
    | default
    = "world",

  systemd.services.hello = {
    wantedBy = ["multi-user.target"],
    serviceConfig.ExecStart = "/usr/bin/hello -g'Hello, %{greeter}!'",
  },
}
```

`greeter` can then be customized without interfering with the final output:

```console
$ nickel export <<< '(import "hello-service.ncl") & {greeter = "country"}'
{
  "systemd": {
    "services": {
      "hello": {
        "serviceConfig": {
          "ExecStart": "/usr/bin/hello -g'Hello, country!'"
        },
        "wantedBy": [
          "multi-user.target"
        ]
      }
    }
  }
}
```

### Documentation

Documentation is attached via the `doc` keyword. Merging propagates
documentation. Documentation can be retrieved through `nickel query`, the
`:query` command inside the REPL, or when using the LSP. For example, we can
query `foo` by running `nickel query --field foo config.ncl` on:

```nickel
# config.ncl
{
  foo | doc "Some documentation"
      | default = {}
} & {
  foo.field = null,
}
```

```console
$ nickel query --field foo config.ncl
• documentation: Some documentation

Available fields
• field
```

If both sides have documentation, the behavior is unspecified, as merging two
distinct blobs of text doesn't make sense in general. Currently, Nickel will
randomly keep one of the two.

## Recursive overriding

We've seen that default values are useful to override a single field with a
different value. The combo of merging, recursive records and merge priorities
together provides the capability of *recursive overriding*, which is a powerful
tool, and the subject of this section.

In Nickel, records are recursive by default, in order to easily express
dependencies between the different fields of a configuration. Concretely, you
can refer to other fields of a record from within the same record:

```nickel
let base_config = {
  version | default = "20.09",
  input.url | default = "nixpkgs/nixos-%{version}",
} in
base_config
```

Here, `version` references the `input` field transparently. This configuration
evaluates to:

```nickel
{
  version = "20.09",
  input = {url = "nixpkgs/nixos-20.09"},
}
```

In this case, when we override the default value of `version`, *the fields that
depend on `version` -- here, `input` -- will also be updated automatically*.
This is what we mean by recursive overriding. For example, `base_config &
{version = "unstable"}` will evaluate to:

```nickel
{
  version = "unstable",
  input = {url = "nixpkgs/nixos-unstable"},
}
```

### Example

Here is another variation of recursive overriding on the `firewall` example:

```nickel
let security = {
  firewall.open_proto.http | default = true,
  firewall.open_proto.https | default = true,
  firewall.open_proto.ftp | default = true,
  firewall.open_ports =
    []
    @ (if firewall.open_proto.ftp then [21] else [])
    @ (if firewall.open_proto.http then [80] else [])
    @ (if firewall.open_proto.https then [443] else []),
}
in # => security.firewall.open_ports = [21, 80, 443]
security & { firewall.open_proto.ftp = false } # => firewall.open_ports = [80, 443]
```

Here, `security.firewall.open_ports` is `[21, 80, 443]`. But in the final
configuration, `firewall.open_ports` will be `[80, 443]`.

[rfc001]: https://github.com/tweag/nickel/blob/c21cf280dc610821fceed4c2caafedb60ce7177c/rfcs/001-overriding.md
