# Merging records

When you have a big configuration, the way to split it in several files
(e.g.: separated by cathegorie or level of abstraction) is the merging operator.
In nickel this is the `&` operator.

For instence, having a server config and a firewall config, the network config
could be:

```text
let
server = import "server.ncl",
firewall = import "firewall.ncl",
in
server & firewall
```

In the simple case where both records do not have common fields, it gives a
record which is the union of both.

If you have to merge records with fields in commons, you will use two new concepts:

- default annotation,
- overwriting,

## Default annotation

To be able to merge records with commons fields, at least one side field has to
be annotated as default. This means, if you have two records:

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
Here, the issue is on the field `firewall.enabled`. What you have to do to fix it
is to annotate this field as default in one of the records:

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
So, this value can be changed afterward. Saying it in a more abstract way,
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

To simplify, when merging two records, all contracts of both left and right one
are applied to the resulting one.
For instence:

```text

let Port | doc "A contract for a port number" = contracts.from_predicate (fun value =>
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
