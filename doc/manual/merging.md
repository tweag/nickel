# Merging records

When you have a big configuration, the way to split it in several files
(e.g.: separated by cathegorie or level of abstraction) is the merging operator.
In nickel this is the `&` operator.

For instance, having a server config and a firewall config, the network config
could be:

```text
let
server = import "server.ncl",
firewall = import "firewall.ncl",
in
server & firewall
```

In the simple case where records `x` and `y` have no common fields, `x&y` is the
union of `x` and `y`.

If you have to merge records with fields in commons, you will have the following cases:

- Merging of fields beeing themself records.
- Merging with records containing fields with merely contracts (without value)
- Merging, with one side annotated `default`

## Recursivity of the merge

When merging, in the case of intersection of fields, Nickel will try to
recursively merge them. That mean: if you have tow records, `x` and `y`, both
having a field `a`; if `a` is a record, the merge will be a record `z` with a
field a beeing the merge between `x.a` and `y.a` or `z = {a = x.a & y.a}.

Example:

```text
// file firewall/udp.ncl
{
    open_ports.udp = [12345,12346], // same as open_ports = {udp = [...]},
}

// file firewall/tcp.ncl
{
    open_ports.tcp = [23, 80, 443], // same as open_ports = {tcp = [...]},
}

// firewall.ncl
let
udp = import "firewall/udp.ncl",
tcp = import "firewall/tcp.ncl",
in
udp & tcp
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

This property can generaly be used to implement mixins like design.

```
let Host = {
    host_name | Str,
    public_addr | Str,
    // return the record depending on host_name and public_addr
    dns_rec: Str -> Str = fun rec_type => rec_type ++ ": " ++ public_addr ++ ", " ++ host_name,
} in
let exemple_dot_com = {
    host_name = "exemple.com",
    public_addr = "0.0.0.0",
} & Host in
exemple_dot_com.dns_rec "A"
```

## Default annotation

If you need the same behaviour but with the field defaulting to a specified
value if not set during any merge, you will probably try something like:

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
Here, the issue is on the field `firewall.enabled`. It's undecidable which value
to keep. Also instead of priorising one to the other, Nickel prefer to be
explicit and provide the `default` annotation:

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
left & right
// => {firewall.enabled = false, ...}
```

The default annotation is generaly to give a default value to a record field.
So, this value can be changed afterward. Saying it in an different way than
the explaination maid in the current part introduction,
default indicate a lower priority to a field in case of merging. Saying that,
If both sides have been annotated `default`, the merge is not possible.

## Overwriting

The overwriting is the concept specifying the behaviour of a merge when you
overwrite a field on which depends an other one. This  feature is described in
["#573 [RFC] Merge types and terms syntax proposal"](https://github.com/tweag/nickel/pull/573)
in more details.

Here, we will simply explain what appen in the following case:
We have a record with some fields annotated `default` and others depending on
these ones. An example could be:

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

We then see that dependent fields are updated when you overwrite the fields they
depend on.

## A word about contracts

When merging two records, all contracts of both left and right one
are applied to the resulting one.
For instance:

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

If in the second record we would have put `port=8888` it does not have blame.
