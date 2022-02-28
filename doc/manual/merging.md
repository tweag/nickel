# Merging records

In Nickel, the basic building blocks for data are records (objects in JSON or
attribute sets in Nix). Merging is a fundamental built-in operation whose role
is to combine records togethers. It is useful to compose small and logical
blocks into a potentially complex final configuration, making it more
manageable. Merge is performed by the `&` operator.

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

- Merging two records without common fields
- Merging records with common fields
- Merging records with metadata
  * Default values
  * Contracts
  * Documentation
- Recursive overriding

## Simple merge (no common fields)

Merging two records with no common fields results in a record with the fields
from both operands. That is, `{foo = 1, bar = "bar"} & {baz =false}` evaluates
to `{foo = 1, bar = "bar", baz = false}`.

### Specification

Technically, if we write the left operand as:

```
letf = {
  field_left_1 = value_left_1,
  ..,
  field_left_n = value_left_n,
}
```

And the right operand as:

```
right {
  field_right_1 = value_right_1,
  ..,
  field_right_k = value_right_k
}
```

Then the merge `left & right` evaluates to the record:

```
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
// file: server.ncl
{
    host_name = "example",
    host = "example.org",
    ip_addr = "0.0.0.0",
}

// file: firewall.ncl
{
    enable_firewall = true,
    open_ports = [23, 80, 443],
}

// file: network.ncl
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
// file: safe-network.ncl
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

Evaluates to the record

```nickel
{
  top_left = 1,
  top_right = 2,
  common = {left = "left", right = "right"}
}
```

When one or both of the common fields are not records, the merge will fail
unless one of the following condition hold:

- They are both of a primitive data type `Num`, `Bool`, `Enum`, or they are null, and they are equal
- They are physically equal, meaning that they point to the same location in
  memory.

The rationale is that only equal values are merged, but for a notion of equality
that immediate to determine for the interpreter, and thus most restrictive than
`==`.

### Specification

```
letf = {
  field_left_1 = value_left_1,
  ..,
  field_left_n = value_left_n,
  common_1 = common_vleft_1,
  ..,
  common_m = common_vleft_m,
}
```

And the right operand as:

```
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

```
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

```
v1 & v2 = v1    if (type_of(v1) is Num, Bool, Str, Enum or v1 == null)
                   AND v1 == v2
          _|_   otherwise (indicates failure)
```

### Example

```nickel
// file: udp.ncl
{
    // same as firewall = {open_ports = {udp = [...]}},
    firewall.open_ports.udp = [12345,12346],
}

// file: tcp.ncl
{
    // same as firewall = {open_ports = {tcp = [...]}},
    firewall.open_ports.tcp = [23, 80, 443],
}

// firewall.ncl
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

## Merging record with metadata

Metadata can be attached to values thanks to the `|` operator. Metadata
currently includes contract annotations, default value, and documentation. We
describe in this section how metadata interacts with merging.

## Default values

A `default` annotation can be used to provide a base value, but let it be
overridable through merging. For example, `{foo | default = 1} & {foo = 2}`
evaluates to `{foo = 2}`. Without the default value, this merge would have
failed with a `non mergeable fields` error, because merging being symmetric, it
doesn't know how to combine `1` and `2` in a generic and meaningful way.

### Specification

We can consider the merging system to feature priorities. To each field
definition `foo = val` is associated a priority `p(val)`. When merging two
common fields `value_left` and `value_right`, then the results is either the one
with the highest priority (that overrides the other), or the two are tentatively
recursively merged, if the priorities are the same. Without loss of generality,
we consider the simple case of two records with only one field, which is the
same on both side:

```
{common = left} & {common = right}
= {
  common = left          if p(left) > p(right)
           right         if p(left) < p(right)
           left & right  if p(left) = p(right)
}
```

Currently, there are only two priorities, `normal` (by default, when nothing is
specified) and the `default` one, with `default < normal`. We plan to add more
in the future (see [RFC001][rfc001]).

### Example

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

```
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

```
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

## Contracts

*Note*: see the [correctness section](./correctness.md) and the
[contracts section](./contracts.md) for a thorough introduction to contracts in
Nickel.

Fields may have contracts attached, either directly, as in `{foo | Num = 1}`, or
propagated from an annotation higher up, as in `{foo = 1} | {foo | Num}`. In
both cases, `foo` must satisfy the contract `Num`. What happens if the value of
`foo` is altered in a subsequent merge? For example:

- Should `{foo | default | Num = 1} & {foo = "bar"}` succeed, although `foo`
  would be a string in the final result?
- Should `{foo.subfield | Str = "a"} & {foo.other_subfield = 1}`
  succeed, although a closed contract `{subfield | Str}` is attached to `foo`,
  and the final result would have an additional field `other_subfield` ?

Nickel chooses to answer **no** to both. In general, when a contract is attached
to a field `foo`, merging ensures that whatever is this field merged with,
including being dropped in favor of another value, the final value for `foo` has
to respect the contract as well or the evaluation will fail accordingly.

### Specification

For two operands with one field each, which the same on both side, with respective
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

### Example

```nickel
let Port
  | doc "A valid port number"
  = contract.from_predicate (fun value =>
    builtin.is_num value &&
    value % 1 == 0 &&
    value >= 0 &&
    value <= 65535) in
let GreaterThan
  | doc "A number greater than the paramater"
  = fun x => contract.from_predicate (fun value => value > x) in

{
    port | GreaterThan 1024
         | default = 8080,
} & {
    port | Port = 80,
}
```

This fails at evaluation:

```
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


## Documentation

Documentation is attached via the `doc` keyword. Documentation is propagated
during merging. For example, querying `foo` by using the command `nickel -f
config.ncl query foo` on:

```nickel
// config.ncl
{
  foo | doc "Some documentation"
      | = {}
} & {
  foo.field = null,
}
```

Will print `"Some documentation"` as expected. If both sides have documentation, the behavior
is unspecified, as merging two distinct blobs of text doesn't always make sense
in general. Currently, Nickel will randomly keeps one of the two in practice.

## Recursive overriding

We've seen in the section on default values that they are useful to override
(update) a single field with a different value. The combo of merging and default
values can do more. In Nickel, records are recursive by default, in order to
express easily dependencies between the different fields of the configuration.
Concretely, you can refer to other fields of a record from within this record:

```nickel
let base_config = {
  version | default = "20.09",
  input.url | default = url = "nixpkgs/nixos-%{version}",
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

Currently, one can only use it on a field that have been marked as default. A
more ergonomic way of overriding is planned, and described in [RFC001][rfc001].

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
} in // => security.firewall.open_ports = [21, 80, 443]
security & {firewall.open_proto.ftp = false} // => firewall.open_ports = [80, 443]
```

Here, `security.firewall.open_ports` is `[21, 80, 443]`. But in the returned
configuration (let's call it `result`), `result.firewall.open_ports = [80, 443]`.

[rfc001]: https://github.com/tweag/nickel/blob/c21cf280dc610821fceed4c2caafedb60ce7177c/rfcs/001-overriding.md
