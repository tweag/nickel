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

In the simple case where both record does not have common fields, it give a
record which is the union of both ones.

If you have to merge with fields in commons, you will use two new concepts:

- default annotation,
- overwriting,

## Default annotation.

To be able to merge records with commons fields, at least one side field has to
be annotated as default. It means, if you have two records:

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
default indicate a smaller priority to a field in case of merging. Saying that,
If both side would have been annotated `default`, the merge is not possible
neither.
