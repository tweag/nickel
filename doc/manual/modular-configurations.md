---
slug: modular-configurations
---

# Modular configurations

This section explains how to leverage Nickel's features, and in
particular the [merge system](./merging.md), to write reusable and maintainable
configuration modules.

## Programmable configuration

One of the starting points of Nickel has been the limits of static data formats
such as JSON, TOML or YAML when tackling large configurations. In particular,
those formats have no way of reusing values of your configuration. Have a single
value used in several places (say, an IP)? Have this virtual machine description
block used several times, almost identically but for a few parameters? In JSON,
the best you can do is to copy and paste, praying for the different copies to not
drift away with time.

In Nickel, you can just use variables and functions:

```nickel
let master_ip = "192.168.1.23" in
let machine = fun {ip, open_ports} => {
  # machine definition depending on ip and open_ports
} in

{
  ip = master_ip,
  cmd = "ssh -p 8070 user@%{master_ip}",
  client1 = machine {ip = "0.0.0.0", open_ports = [22]},
  client2 = machine {ip = "1.1.1.1", open_ports = [80]},
}
```

In the zoology of programming abstractions, functions are the simplest and most
ubiquitous concept of all. For the machine example, a ([pure][pure-function])
function is exactly what we look for: an expression with unknown parameters
which can be selected at usage time. Virtually every programming language has
functions in some form, even if they're not called that (for example, in
Terraform's configuration language, _modules_ are more or less
functions[^terraform-modules]).

You might even want to not hardcode some parameters directly in the
configuration but rather fetch them from the command line or an environment
variable for each deployment, turning your whole configuration
to a function:

```nickel
fun {param1, param2, ..} =>
{
  # your config
}
```

This pattern probably looks familiar to [Nix][nix] developers: this is exactly
how a Nix package is defined, where arguments are other packages (programs,
tools and libraries) this package depends on.

Functions are simple yet powerful and flexible. They can be composed, passed
around, returned... so, aren't functions sufficient?

[^terraform-modules]:
    According to [Terraform's own
    manual][terraform-modules-manual]

## Issues with functions

Functions might actually not be the best solution for reusable configuration
parts, for the following reasons.

### Functions are opaque

Functions are opaque values that need to be fed arguments before producing any
data. If your whole configuration is now a function, you can't manipulate or
explore it anymore. You need to provide arguments first.

A basic example is the `nickel query` subcommand, which extracts information
and documentation from a configuration (the example comes from the [Nickel
repository][nickel-repo]):

```console
$ nickel query examples/config-gcc/config-gcc.ncl
Available fields
• flags
• optimization_level
• path_libc

$ nickel query examples/config-gcc/config-gcc.ncl path_libc
• contract: Path,SharedObjectFile
• default: "/lib/x86_64-linux-gnu/libc.so"
• documentation: Path to libc.
```

But `query` doesn't work on functions!

<!-- Unfortunately, the manual snippet checker can't handle REPL commands such
as :query, so we disable it for the following snippet -->
```nickel #no-check
> let example1 =
  let value = 1 in {left = "a", right = 1}

> :query example1
Available fields
• left
• right

> let example2 =
  fun param => let value = 1 in {left = param, right = 1}

> :query example2
```

`query` can perform some evaluation and sees through the let-binding in the
first example, but it can't evaluate inside the body of a function before it's
applied.

This is frustrating, because most of the time, 90% of the structure and the
content of the configuration is known while just a few values depend on the
parameters. This is a well-known issue in Nix[^nix-pkgs-as-functions].

[^nix-pkgs-as-functions]: [Nix language changes, _Eelco Dolstra_][eelco-nix-lang-changes]

### Functions aren't always nice to combine

I previously said that functions were composable, and I stand by this statement
in general. Functions lend themselves particularly well to sequential
composition, that is piping several operations one after the other. Applying
array operations in Nickel is indeed particularly pleasant[^pipe-operator]:

```nickel #repl
> let do_stuff : Array Number -> Number
    = fun array =>
      array
      # keep odd numbers
      |> std.array.filter (fun x => x % 2 == 1)
      # add one
      |> std.array.map ((+) 1)
      # multiply them together
      |> std.array.reduce_left (*)
    in
  do_stuff [1, 2, 3, 4, 5]
48
```

However, when writing configurations, it's not that common to "pipe" many
configuration parts together this way (we do pipe operations to generate
_individual values_ as in the example above - that's why Nickel has functions,
they are great for that). Instead, we want to paste many configuration parts.
Pasting can be done using Nickel's [merge operator `&`][merge-manual-page].
Merge works well on its own, but using functions for abstraction is still
awkward. Consider for example:

```nickel #no-check
# network.ncl
fun {ip, firewall_enabled, ipv6_enabled} => { .. }
# services.ncl
fun {httpd_enabled, openssh_version} => { .. }
# volumes.ncl
fun {volume_max_size, lvm_enabled} => { .. }
```

If we want to combine those parts, we have to glue everything together:

<!-- We have to disable checks here because the imports would fail -->
```nickel #no-check
let network = import "network.ncl" in
let services = import "services.ncl" in
let volumes = import "volumes.ncl" in

fun {
  ip,
  firewall_enabled,
  ipv6_enabled,
  httpd_enabled,
  openssh_version,
  volume_max_size,
  lvm_enabled,
} =>
  network { ip, firewall_enabled, ipv6_enabled }
  & services { httpd_enabled, openssh_version }
  & volumes { volume_max_size, lvm_enabled }
```

The result is mostly boilerplate repeating information we already know. If you
need different combinations somewhere else, such as `network` and `services`
only, you need to write all the corresponding wrapper functions.

Functions are nice to compose sequentially but not as nice to compose _in
parallel_, which is what we do here.

[^pipe-operator]:
    the pipe operator `|>` can appear magical, but it's just
    syntactic sugar for reverse application, that is `data |> f |> g` is `g (f data)`

### Functions are hard to override

Overriding is the act of taking an existing configuration (in our case a
function that may have already been applied) but tweaking just one parameter. Isn't
overriding the essence of configuration, after all? Toggle a flag, replace a
value, and run again.

Overriding turns out to be a hard problem when configuration is built from
composing functions. If you control the source code, you can change the value
provided to the configuration-as-a-function at the call-site. But if this function
calls other functions internally, the parameters to the inner call aren't
necessarily accessible. You need to bubble them up to the root function's
argument.

It also happens that one doesn't control the call-site or the function's code,
because it's in a library. That is, overriding is **opt-in for the author of the
function**, but the one who needs it is the **consumer**. It's hard for the
author to know in advance everything that could be useful to expose for
overriding.

Overriding is typically an issue in Nix: when pulling package descriptions from
the central repository ([Nixpkgs][nixpkgs]), the package is "already applied".
For example, we build Nickel with Nix, and we need to make sure the version of
`wasm-bindgen` we pull from Nixpkgs is the same as the one set in `Cargo.toml`.
A naive way would be to do something like (in Nix, `//` overrides a value in a
record):

```nix
wasm_bindgen_cli =
  pkgs.wasm-bindgen-cli // {version = wasm_bindgen_fixed_version;}
```

Sadly, this doesn't work. This code overrides the final `version` field
correctly, but not the other values which depend on version (such as `src`).
What we want to override is the _original_ `version` used when calling the
package-as-a-function, not the one propagated in the end result. But once an
arbitrary function has been applied, there's no coming back.

The [actual Nix code][wasm-bindgen-override] to build Nickel calls a
mysterious `override` function, which is a bespoke mechanism of Nixpkgs to
simulate this kind of overriding. Spoiler: there are several competing such
overriding mechanisms in Nix, they are all hard to wrap your head around, and
you can't always override everything you would like to.

## Merging system

In Nickel, we advocate for a different model to write and assemble configuration
parts. Instead of writing functions that return records and pipe them together,
we write records directly where all fields might not have a definition _yet_. We
refer to configurations written in this style as _partial configurations_.

Nickel's records are recursive, meaning that a field definition may depend on
other fields of the same record:

```nickel #parse
# machine.ncl
{
  ip,
  cmd = "ssh -p 8070 user@%{ip}",
}
```

Here, `ip` doesn't have a definition. This configuration is partial and
can't be exported yet:

```console
$ nickel export machine.ncl
error: missing definition for `ip`
  ┌─ machine.ncl:2:3
  │
1 │ ╭ {
2 │ │   ip,
  │ │   ^^ required here
3 │ │   cmd = "ssh -p 8070 user@%{ip}",
4 │ │ }
  │ ╰─' in this record
```

However, merging this snippet with a record defining `ip` does the trick:

```console
$ echo '(import "machine.ncl") & {ip = "1.1.1.1"}' > config.ncl
$ nickel export config.ncl
{
  "ip": "1.1.1.1"
  "cmd": "ssh -p 8070 user@1.1.1.1",
}
```

Deep down, recursive records could be seen as just a fancy representation for
functions (in the same way as [objects are poor man's
closures][objects-closures]). But we'll see how this representation makes many
configuration-related operations much easier.

### Records are transparent

As opposed to functions, partial records don't need to hang on their missing
field. We can extract documentation from them, get completion in the LSP, use
`nickel query`, and so on. The structure of the final configuration is already
apparent: there are just some holes to be filled.

### Records remember

As opposed to functions, recursive records remember the relation between their
different fields even if they have all been given a value at this point.
Overriding a field will automatically recompute the new value of its reverse
dependencies. Remember the `wasm-bindgen` example, where we had to use Nixpkgs'
`override`. In Nickel, you don't need any of that. The following is a similar
example, albeit simplified, where the `cmd` field depends on the `ip` field
(similar to `version` in the `wasm-bindgen` example). In the partial
configuration model, you can naively override `ip`:

```nickel
# machines.ncl
let init_machine = {
  ip = "1.1.1.1",
  cmd = "ssh -p 8070 user@%{ip}",
} in

{
  old = init_machine,
  new = old & { ip | force = "198.162.0.1" },
}
```

`ip` is properly updated in `new`:

```console
$ nickel export packages.ncl
{
  "old": {
    "ip": "1.1.1.1"
    "cmd": "ssh -p 8070 user@1.1.1.1",
  },
  "new": {
    "ip": "198.162.0.1"
    "cmd": "ssh -p 8070 user@198.162.0.1",
  },
}
```

### Records are nice to combine

Finally, recall the example from _Functions aren't always nice to combine_ with
a `network` part, a `services` part and a `volumes` part.

We had to redeclare the full list of parameters and correctly forward them to
subfunctions.

With records, you can just merge them. In some sense, merge is like parallel
composition: it pastes records together, the fields without definition in the
result (the arguments) being the union of the fields without definition from
each part.

```nickel #parse
{
  all = network & services & volumes,
  partial = network & services,
}
```

## Toward modules

Nickel is centered around records as the primary unit of configuration. Metadata
can be attached to record fields, giving them more expressive power and the
ability to better describe an _interface_ for a partial configuration. Combined
with field metadata, you can use records - that is, partial configurations - as
a reusable configuration module. For example:

```nickel #parse
{
  inputs | not_exported = {
      foo
        | String
        | doc "Doc of foo",
      bar
        | Number
        | doc "Doc of bar",
      unused
        | Bool
        | optional,
    },
  local | not_exported = {
      computed = std.string.lowercase inputs.foo,
    },

  some_config_option = inputs.bar + 1,
  other_option = std.string.join " " ["Hello", local.computed],
  last_option = "values are %{local.computed} and %{std.to_string inputs.bar}",
}
```

We used the `not_exported` metadata to signal that the `inputs` and `local`
fields are high-level values that are used to generate the configuration, but
they shouldn't be part of the final export. `inputs` is supposed to be set by
consumers of this module. `local` contains intermediate values that
are computed from the inputs and possibly reused many times in the outputs.

Finally, the outputs lie at the root of the record. We could also have put them
under an `output` field, requiring to extract the `output` field in the end
(but, in return, we can then remove the `not_exported`).

All of those values are readily and automatically overridable (using the `force`
metadata for fields that already have a non-default value), and changes are
propagated to reverse dependencies accordingly. The proposed layout is rather
arbitrary: Nickel has no notion of inputs, local and outputs, but rather of
fields with and without definition as well as priorities.

The metadata associated to `inputs` define a clear typed module interface which
can be leveraged by the Nickel tooling.

Nickel might get a first-class notion of modules one day, if only to enforce a
standard structure across projects. But, as far as functionality is concerned,
the current merge system can already get you pretty far in an intuitive way.

[pure-function]: https://en.wikipedia.org/wiki/Pure_function
[nix]: https://nixos.org/
[nixpkgs]: https://github.com/NixOS/nixpkgs
[wasm-bindgen-override]: https://github.com/tweag/nickel/blob/292c7ff6a04f066c542fa6290dc2677393b7a2bc/flake.nix#L86
[nickel-repo]: https://github.com/tweag/nickel
[objects-closures]: http://wiki.c2.com/?ClosuresAndObjectsAreEquivalent
[merge-manual-page]: https://nickel-lang.org/user-manual/merging
[eelco-nix-lang-changes]: https://gist.github.com/edolstra/29ce9d8ea399b703a7023073b0dbc00d
[terraform-modules-manual]: https://developer.hashicorp.com/terraform/language/v1.6.x/values/variables
