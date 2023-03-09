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

Merge is a **symmetric** operation (or, pedantically, commutative). In practice,
this means that order doesn't matter, and `left & right` is the same thing as
`right & left`. When the operands need to be distinguished, as we will see for
default values for example, the idea is to use metadata to do so (annotations),
rather than relying on the left or right position.

**Warning**: At the time of writing, Nickel's version is 0.1. Important
additions to merging are planned for coming versions, including priorities and
custom merge functions. They are not detailed here yet. For more details, see
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
from both operands. That is, `{foo = 1, bar = "bar"} & {baz =false}` evaluates
to `{foo = 1, bar = "bar", baz = false}`.

### Specification

Technically, if we write the left operand as:

```text
left = {
  field_left_1 = value_left_1,
  ..,
  field_left_n = value_left_n,
}
```

And the right operand as:

```text
right {
  field_right_1 = value_right_1,
  ..,
  field_right_k = value_right_k
}
```

Then the merge `left & right` evaluates to the record:

```text
{
  field_left_1 = value_left_1,
  ..,
  field_left_n = value_left_n,
  field_right_1 = value_right_1,
  ..,
  field_right_k = value_right_k
}
```

In other terms, `left & right` is the union of `left` and `right`.

### Examples

#### Split

You can split a configuration into subdomains:

```nickel
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

```nickel
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

- They are both of a primitive data type `Number`, `Bool`, `Enum`, `String` and
  they are equal
- They are both null

### Specification

```text
left = {
  field_left_1 = value_left_1,
  ..,
  field_left_n = value_left_n,
  common_1 = common_vleft_1,
  ..,
  common_m = common_vleft_m,
}
```

And the right operand as:

```text
right {
  field_right_1 = value_right_1,
  ..,
  field_right_k = value_right_k
  common_1 = common_vright_1,
  ..,
  common_m = common_vright_m,
}
```

Where the `field_left_i` and `field_right_j` are distinct for all `i` and `j`.
Then the merge `left & right` evaluates to the record:

```text
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
v1 & v2 = v1    if (type_of(v1) is Number, Bool, String, Enum or v1 == null)
                   AND v1 == v2
          _|_   otherwise (indicates failure)
```

### Example

```nickel
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
both sides, the value is a record, which is therefore merged. The same process
happens one layer below, on the common field `open_ports`, to result in the
final record:

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
currently includes contract annotations, default value, merge priority, and
documentation. We describe in this section how metadata interacts with merging.

### Optional fields

A field can be marked as optional using the `optional` annotation:

```nickel
# schema.ncl
{
  Command = {
    command
      | String,
    arg_type
      | [| `String, `Number |],
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
let command | Command = {
  command = "exit",
  arg_type = `String,
  alias = "e",
}
in

command
```

Once an optional field becomes defined, it just acts like a regular field. Any
attached contract will be applied as well (here, `String` on `alias`).

As long as an optional field doesn't have a value, it will be invisible to
record operations. Optional fields without a value don't show up in
`record.fields`, it won't make `record.values` throw a missing field definition
error, etc.

```nickel
nickel> let Contract = {foo = 1, bar | optional}
nickel> record.values Contract
[ 1 ]

nickel> record.has_field "bar" Contract
false
```

Optional field can still be discovered through metadata queries (run `nickel
help query` or type `:help query` in the REPL for more information) or generated
documentation.

An optional field becomes a regular field as soon as it is merged with the same
field that doesn't have the `optional` annotation. This holds **even if the
other field doesn't have a definition**:

```nickel
nickel>  {foo = 1, bar | optional} & {bar | optional}
{ foo = 1 }

nickel> {foo = 1, bar | optional} & {bar}
error: missing definition for `bar`
  ┌─ repl-input-4:1:1
  │
1 │ {foo = 1, bar | optional} & {bar, baz = bar + 1}
  │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  │ │         │
  │ │         defined here
  │ in this record
```

In the second example, `bar` isn't optional on the right-hand side of the merge.
Although the right-hand side `bar` doesn't have a definition, the resulting
`bar` field isn't optional anymore.

[^optional-with-value]: You can actually provide a value (default or not) for an optional
  field, but this nullifies the effect of `optional`: what you get is just a
  regular field

### Merge priorities

Priorities are specified using the `priority` annotation, followed by a number
literal. There are also two other special priorities, the bottom priority, specified
using the `default` annotation, and the top priority, specified using the
`force` annotation.

Priorities dictate which values take precedence over other values. By default,
values are given the priority `0`. Values with the same priority are recursively
merged as specified in this document, which can mean failure if the values can't
be meaningfully merged:

```text
nickel> {foo = 1} & {foo = 2}
error: non mergeable terms
  ┌─ repl-input-1:1:8
  │
1 │ {foo = 1} & {foo = 2}
  │        ^           ^ with this expression
  │        │
  │        cannot merge this expression
  │
  = Both values have the same merge priority but they can't be combined
```

On the other hand, if the priorities differ, the value with highest priority
simply erases the other in the final result:

```text
nickel> {foo | priority 1 = 1} & {foo = 2}
{ foo = 1 }

nickel> {foo | priority -1 = 1} & {foo = 2}
{ foo = 2 }
```

The priorities are ordered in the following way:

- bottom is the lowest priority
- numeral priorities are ordered as usual numbers (priorities can be any valid Nickel
  number, including fractions and negative values)
- top is the highest priority

#### Default values

A `default` annotation can be used to provide a base value, but let it be
overridable through merging. For example, `{foo | default = 1} & {foo = 2}`
evaluates to `{foo = 2}`. A default value is just a special case of a priority,
being the lowest possible one.

#### Forcing values

Dually, values with the `force` annotation are given the highest priority. Such
a value can never be overridden, and will either take precedence over another
value or be tentatively merged if the other value is forcing as well.

#### Recursive priorities

**Warning**: the syntax and semantics of recursive priorities are new as of
version 0.3, and may evolve in the future.

Using the `default` annotation on a record value means that the value is either
picked as a block, or erased altogether:

```text
nickel> {server | default = {ip = "192.168.1.1", port = 80}} & {server.ip =
"42.42.42.42"}
{ server = { ip = "42.42.42.42" } }
```

This might not be what you want, for example because the server field should
always define a `port`. It's useful to provide a set of default values that are
individually overridable: in our example, we would like to be able to override
`server` as we did while still keeping a default value for `port`. One solution
is to simply annotate each value with a `default`:

```nickel
{server = {ip | default = "192.168.1.1", port | default = 80}}
```

However, doing so can quickly become cumbersome for larger sets of default
values, where we end up duplicating `default` annotations. We can do better
thanks to *recursive priorities*, written `rec default` and `rec force`. During
evaluation, recursive priorities are dynamically pushed down until they reach
values which are not records anymore, that is to the leaves of the record, so to
speak:

```text
nickel> {server | rec default = {ip = "192.168.1.1", port = 80}} & {server.ip =
"42.42.42.42"}
{ server = { ip = "42.42.42.42", port = 80 } }
```

`rec force` implements the same idea for `force`. It makes overriding happen for
individual fields of a record and not as a block.

If a value already has a merge priority set, the rules are the following:

- `rec default` doesn't apply on values with explicit priorities set. That is,
  `{foo | rec default = {bar | priority 10 = 1, baz = 2}}` will evaluate to
  `{foo = {bar | priority 10 = 1, baz | default = 2}}`.
- `rec force` always overrides existing priorities. That is,
  `{foo | rec force = {bar | priority 10 = 1, baz | default = 2}}` will evaluate
  to `{foo = {bar | force = 1, baz | force = 2}}`.

Put differently, when another priority is explicitly set, we take the maximum
priority between the recursive priority and the existing one.

#### Specification

We can consider the merging system to feature priorities. To each field
definition `foo = val` is associated a priority `p(val)`. When merging two
common fields `value_left` and `value_right`, then the results is either the one
with the highest priority (that overrides the other), or the two are tentatively
recursively merged, if the priorities are the same. Without loss of generality,
we consider the simple case of two records with only one field, which is the
same on both side:

```text
{common = left} & {common = right}
= {
  common = left          if p(left) > p(right)
           right         if p(left) < p(right)
           left & right  if p(left) = p(right)
}
```

#### Example

Let us stick to our firewall example. Thanks to default values, we set the most
restrictive configuration by default, which can still be overridden if needed.

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
thus fail:

```text
error: non mergeable terms
  ┌─ repl-input-0:2:22
  │
2 │   firewall.enabled = true,
  │                      ^^^^ cannot merge this expression
  ·
7 │   firewall.enabled = false,
  │                      ^^^^^ with this expression

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
      "options": "TLS"
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
to a field `foo`, merging ensures that whatever is this field merged with,
including being dropped in favor of another value, the final value for `foo` has
to respect the contract as well or the evaluation will fail accordingly.

#### Specification

For two operands with one field each, which is the same on both side, with respective
contracts `Left1, .., Leftn` and `Right1, .., Rightk` attached:

```nickel
left = {
  common | Left1
         | ..
         | Leftn
}
```

And

```nickel
right = {
  common | Right1
         | ..
         | Rightk
}
```

Then the `common` field of `left & right` will be checked against `Left1, ..,
Leftn, Right1, .., Rightk`. Here, we ignore the case of type annotations such as
`common: LeftType` that can just be considered as an additional contract
`Left0`.

#### Example

```nickel
let Port
  | doc "A valid port number"
  = contract.from_predicate (fun value =>
    builtin.is_number value &&
    value % 1 == 0 &&
    value >= 0 &&
    value <= 65535) in
let GreaterThan
  | doc "A number greater than the parameter"
  = fun x => contract.from_predicate (fun value => value > x) in

{
    port | GreaterThan 1024
         | default = 8080,
} & {
    port | Port = 80,
}
```

This fails at evaluation:

```text
error: contract broken by a value.
[..]
   ┌─ repl-input-1:16:19
   │
16 │     port | Port = 80,
   │                   ^^ applied to this expression

note:
   ┌─ repl-input-1:13:12
   │
13 │     port | GreaterThan 1024
   │            ^^^^^^^^^^^^^^^^ bound here
```

### Not exported

A field can be marked as not exported, using the `not_exported` annotation. Such
fields behave like regular fields during evaluation, but they are ignored during
serialization: they won't appear in the output of `nickel export`, nor in the
output of `builtin.serialize`.

For example, say we want to add some high-level configuration field to a modular
configuration, from which other fields are derived:

```nickel
# hello-service.ncl
{
  greeter
    | String
    | not_exported
    | default = "world",

  systemd.services.hello = {
    wantedBy = ["multi-user.target"],
    serviceConfig.ExecStart = "/usr/bin/hello -g'Hello, %{greeter}!'",
  },
}
```

`greeter` can then be customized without interfering with the final output:

```console
$ nickel export <<< 'import "hello-service.ncl" & {greeter = "country"}'
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

Documentation is attached via the `doc` keyword. Documentation is propagated
during merging. For example, querying `foo` by using the command `nickel -f
config.ncl query foo` on:

```nickel
# config.ncl
{
  foo | doc "Some documentation"
      | = {}
} & {
  foo.field = null,
}
```

Will print `"Some documentation"` as expected. If both sides have documentation,
the behavior is unspecified, as merging two distinct blobs of text doesn't make
sense in general. Currently, Nickel will randomly keeps one of the two in
practice.

## Recursive overriding

We've seen in the section on default values that they are useful to override
(update) a single field with a different value. The combo of merging and
priorities can do more. In Nickel, records are recursive by default, in order to
express easily dependencies between the different fields of the configuration.
Concretely, you can refer to other fields of a record from within this record:

```nickel
let base_config = {
  version | default = "20.09",
  input.url | default = "nixpkgs/nixos-%{version}",
} in
base_config
```

Here, we referred to `version` from the `input` field transparently. This
configuration evaluates to:

```nickel
{
  version = "20.09",
  input = {url = "nixpkgs/nixos-20.09"},
}
```

Merging handles overriding on recursive record too. More precisely, when we
override the default value of `version`, *the fields that depend on `version` --
here, `input` -- will also be updated automatically*. For example, `base_config
& {version = "unstable"}` will evaluate to:

```nickel
{
  version = "unstable",
  input = {url = "nixpkgs/nixos-unstable"},
}
```

### Example

Here is another variation of recursive overriding on our `firewall` example:

```nickel
let security = {
    firewall.open_proto.http | default = true,
    firewall.open_proto.https | default = true,
    firewall.open_proto.ftp | default = true,
    firewall.open_ports = []
        @ (if firewall.open_proto.ftp then [21] else [])
        @ (if firewall.open_proto.http then [80] else [])
        @ (if firewall.open_proto.https then [443] else []),
} in # => security.firewall.open_ports = [21, 80, 443]
security & {firewall.open_proto.ftp = false} # => firewall.open_ports = [80, 443]
```

Here, `security.firewall.open_ports` is `[21, 80, 443]`. But in the returned
configuration (let's call it `result`), `result.firewall.open_ports = [80, 443]`.

[rfc001]: https://github.com/tweag/nickel/blob/c21cf280dc610821fceed4c2caafedb60ce7177c/rfcs/001-overriding.md
