# Merging records

Merging in Nickel allows to compose small, logical, and manageable blocks into a
potentially complex final configuration.

In this part we will try to described this concept exaustively as possible.

Warning: The given examples are clearly not the only context on which merging
can be used. But it's impossible to expose all the cases in a user manual.
You can check the examples in github for further readings.

Warning: Nickel beeing in a pre 1st release state now. Merging is a feature
which can recieve breacking updates until passing in 1.0.0.

In the simple case, you will merge records without commons fields.
If so, the merge is the union between both records.

If you have to merge records with fields in commons, you could be in following cases:

- Merging of fields beeing themself records.
- Merging with records containing fields with merely contracts (without value)
- Merging, with one side annotated `default`

## Simple merge (records without intersection)

The simplest merging case is when both records does not have any common fields.
It can be used to merge records from differents files in one only record.

### Usecases

You generaly will use this feature to be able to split a config in subparts.
When your config has a small set of big subparts, they can themself be either
a one top key record but also a multikeys record. The idea is to group the top
level records by topic.

### Description

Having two records, `x` and `y`. With `x = {x0 = vx0, x1 = vx1, ..., xn = vxn}` and
`y = {y0 = vy0, y1 = vy1, ..., yn = vyn}`. The merge of both give:
`x & y = {x0 = vx0, y0 = vy0, x1 = vx1, y1 = vy1, ..., xn = vxn, yn = vyn}`
In other terms, `x & y` is the union of `x` and `y`.

### Example

For instance, having a server config and a firewall config, the network config
could be:

```text
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

In this simple case where records `x` and `y` have no common fields, `x&y` is the
union of `x` and `y`:

```text
{
    host_name = "example",
    host = "example.org",
    ip_addr = "0.0.0.0",
    enable_firewall = true,
    open_ports = [23, 80, 443],
}
```

## Recursivity of the merge

It's a behaviour you will use in similar cases as before but when the split is
at a deeper level.

### Usecases

In two records you have unintersecting fields but with common
"root path" (e.g.: `open_ports.` as in following example; can also even be
deeper as `firewall.open_ports.`).

### Description

When merging, in the case of intersection of fields, Nickel will try to
recursively merge them. That mean: if you have tow records, `x` and `y`, both
having a field `a`; if `a` is a record, the merge will be a record `z` with a
field a beeing the merge between `x.a` and `y.a` or `z = {a = x.a & y.a}.

### Example

```text
// file: firewall/udp.ncl
{
    open_ports.udp = [12345,12346], // same as open_ports = {udp = [...]},
}

// file: firewall/tcp.ncl
{
    open_ports.tcp = [23, 80, 443], // same as open_ports = {tcp = [...]},
}

// firewall.ncl
let udp = import "firewall/udp.ncl" in
let tcp = import "firewall/tcp.ncl" in
{ firewall= udp & tcp}
```

In the above example, we merge two records, both with a field `open_ports`. The
thing is that, in  both sides, this is itself a record. So, the resulting merge
will have the form:

```text
{
    open_ports = udp.open_ports & tcp.open_ports, // => {udp = [...], tcp = [...]}
}
```

## Records as mixins

To be merged with others, records don't have to have a value attached to every fields.
A record with only contracts fields is perfectly valid also.

### Usecases

This property can generaly be used to implement mixins like design. You can even
write OO like code as well as perform traits like implementation on records.

### Description

In Nickel, all or a part of the fields of a record can be uninitialized and
contain only contracts. In this state, they can not be evaluated. However, after
a merge with another record containing the missing values, the result can be
evaluated normaly and contracts will be checked on the seted values. Moreover
some field of a record can depend on not initialized ones. Because of Nickel lazyness
while not evaluated, no error will be thrown. When evaluating an uninitialized field,
Nickel throw a `EmptyMetavalue` error.

### Example

```text
let Host = {
    host_name | Str,
    public_addr | Str,
    // return the record depending on host_name and public_addr
    dns_rec: Str -> Str = fun rec_type =>
        rec_type ++ ": " ++ public_addr ++ ", " ++ host_name,
} in
let exemple_dot_org = {
    host_name = "exemple.org",
    public_addr = "0.0.0.0",
} & Host in
exemple_dot_org.dns_rec "A"
```

Above, the defined field `dns_rec` is a parametrised function. But,
As, you will see in the overwriting part, it could have been a recursively
depend field. Actualy, Nickel beeing a lazily functional language, a
variable can be seen as a function without params.

## Default annotation

If you need the same behaviour but with the field defaulting to a specified
value if not set during any merge, `default` annotation is the answer.

### Usecases

The default annotation is generaly to give a default value to a record field.
The main usage difference between using valueless fields with defaulting fields is
that the first make a field requiered to have a valid config where the second
make it "optionaly updatable". Even more, giving a value to a field make it
"read only" if `default is not set.

### Description

A value tagged `default` can be updated afterward. Saying it in an different way
than the explaination maid in the usecases part,
default indicate a lower priority to a field in case of merging. Saying that,
If both sides have been annotated `default` with both attached to a value, the
merge is not possible.

In other words, Nickel has two level of priority when merging. This mainly for
two reasons:

- instead of priorising one record to the other, Nickel prefer to be
  explicit and provide the `default` annotation,
- finaly, when not annotated, it make fields read only by default (when initialized)
  which is more secure.

### Example

One more time we can give a firewall example which will have the most restrictives
values by default and can be updated".
But first, let's check what append if we forget the `default`:

```text
let left = {
  firewall.enabled = true,
  firewall.type = "iptables",
  firewall.open_ports = [21, 80, 443],
} in
let right = {
  firewall.enabled = false,
  server.host.options = "TLS",
} in
left & right
```

Like it is, it's impossible to merge and will throw an unmergeable terms error.
Here, the issue is that the field `firewall.enabled` is defined in both sides:
It's undecidable which value to keep,

The solution here is:

```text
let left = {
  firewall.enabled | default = true,
  firewall.type = "iptables",
  firewall.open_ports = [21, 80, 443],
} in
let right = {
  firewall.enabled = false,
  server.host.options = "TLS",
} in
left & right // => {firewall.enabled = false, ...}
```

## Overwriting

The overwriting is the concept specifying the behaviour of a merge when you
overwrite a field on which depends an other one. This  feature is described in
RFC001 for further readings.

### Usecases

In short you can see it as a mix between the two previous parts. A record with
some fields annotated `default` and others depending on these ones.
Actualy if you refer to "Mixins" part, you can rewrite the examples with `default`
values and the behaviour will be overwriting.

### Description

The important thing to notice is that, the depend fields are updated as soon as
fields on which they depend on are updated.

### Example

```text
let security = {
    firewall.open_proto.http | default = true,
    firewall.open_proto.https | default = true,
    firewall.open_proto.ftp | default = true,
    firewall.open_ports = []
        @ (if firewall.open_proto.ftp then [21] else [])
	@ (if firewall.open_proto.http then [80] else [])
	@ (if firewall.open_proto.https then [443] else []),
} in // security => {firewall.open_ports = [21, 80, 443]
security & {firewall.open_proto.ftp = false} // => {firewall.open_ports = [80, 443]
```

Above, you can notice that, if accessing `security.firewall.open_ports` before
the merge, it will have a value. After the merge, this value is actuated.

In the Mixins part, we used a function field. It give the same here. We used simple
depend field for clarity but both behave the same.

## A word about contracts

### Contracts crossvalidation

When merging records with a common field foo and different contracts attached on
each side (say `A1`, `A2` on the left and `B` on the right), then merging
ensures that the final value will respect the combination of all the contracts
(`A1`, `A2`, `B`). For instance:

```text
let Port | doc "A contract for a port number"
  = contracts.from_predicate (fun value =>
  builtins.is_num value &&
  value % 1 == 0 &&
  value >= 0 &&
  value <= 65535) in
let Gt | doc "Contract greater than" = fun x =>
contracts.from_predicate(fun value =>
value > x) in
{
    port | #(Gt 1024) | default = 8080,
} & {
    port | #Port = 80,
} // blame because 80 < 1024
```

In the case the second record would contains `port=8888` it would not have blame.

### Case of `doc`

Another annotation which has to be managed during merging is the `doc` annotation.
When merging records both with documentation, only the most left one is keped:

```text
let x | doc "x" = {a = 1} in
let y | doc "y" = {b = 2} in
x & y // doc contains "x"
```

The behaviour is the same for interior field annotation:

```text
let x = {field | doc "field x" | Num} in
let y = {field | doc "field y" = 2} in
x & y // doc contains "field x"
```

You can try reversing like this `y & x`. The doc will contains "field y".

TODO: what's more?
