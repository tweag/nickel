---
page: "getting-started"
---

# Getting started

Nickel is quite new and not yet distributed using the standard channels
(binaries, nix package, Rust crate, and so on). We are sorry if the installation
process is not yet optimal, but this should change soon, so stay tuned.

## Build from source using Nix

Using [Nix]("https://nixos.org/") is the easiest way to get a Nickel executable
running:

1. Clone the [Nickel repository](https://github.io/tweag/nickel)
   locally and set it as the current directory:

   ```shell-session
   $ git clone git@github.com:tweag/nickel.git
   Cloning in 'nickel'...
   [..]
   $ cd nickel
   devops@nickel-lang:~/nickel$ 
   ```

1. Invoke nix build:

   ```shell-session
   devops@nickel-lang:~/nickel$ nix build
   [1 built, 0.0 MiB DL]
   devops@nickel-lang:~/nickel$ 
   ```

1. If everything went right, a binary is now available in the
   result directory:

   ```shell-session
   devops@nickel-lang:~/nickel$ ./result/bin/nickel -V
   nickel 0.1.0
   devops@nickel-lang:~/nickel$  
   ```

## Build from source without Nix

You will find alternative ways to build Nickel from source by cloning the
[repository](href="https://github.io/tweag/nickel) and following the
instructions of the
[README](href="https://github.com/tweag/nickel/#getting-started").

## Write your first configuration

 Nickel has a ton of cool features, like gradual typing, contracts and a merge
 system. However, you'll only have to deal with them once you need them. Writing
 basic configuration is almost as writing JSON or YAML. Let us start with a
 basic fictional app configuration:

```nickel
{
  name = "example",
  description = m#"
    This is an awesome software I'm developing.
    Please use it!
  "#m,
  version = "0.1.1",
  main = "index.js",
  keywords = ["example", "config"],
  scripts = {
    test = m#"test.sh --option --install example --version "0.1.1""#m,
    do_stuff = "do_stuff.sh subcommand",
  },
  contributors = [{
      name = "John Doe",
      email = "johndoe@example.com"
    }, {
      name = "Ivy Lane",
      url = "https=//example.com/ivylane"
    }],
  dependencies = {
    dep1 = "^1.0.0",
    dep3 = "6.7"
  }
}
```

This program describe a record delimited by `{` and `}`, consisting in a list of
key-value pairs, akin to JSON's objects. Nickel basic datatypes include strings
delimited by `"` and lists, by `[` and `]`.

The m#" and "#m delimits multiline strings. In such strings, the common
indentation prefix is stripped, and special characters (excepted
interpolation #{}) loose their meaning. It is useful for two purpose
illustrated here:

- Writing strings spanning multiple lines while keeping the same
  indentation as code.
- Writing strings with special characters in it, without having to
  escape them (", \, and so on).

## Export

Now, save the content in "example.ncl" and run nickel export (or
./result/bin/nickel export if you haven't made a symbolic link):

```shell-session
devops@nickel-lang:~/nickel$ nickel -f example.ncl export --format yaml
---
contributors:
  - email: johndoe@example.com
    name: John Doe
  - name: Ivy Lane
    url: https=//example.com/ivylane
dependencies:
  dep1: ^1.0.0
  dep3: "6.7"
description: "This is an awesome software I'm developing.\nPlease use it!"
keywords:
  - example
  - config
main: index.js
name: example
scripts:
  do_stuff: do_stuff.sh subcommand
  test: "test.sh --option --install example --version \"0.1.1\""
version: 0.1.1
```

Currently supported formats are yaml, toml, json, and raw. json is the
default, while raw expect a string result that it output directly, useful to
generate e.g. shell scripts or other custom data.

## Reuse

Nickel is a programming language. This allows you not only to describe, but to
generate data. There's some repetition in our previous example (reproducing only
the interesting part):

```nickel
name = "example",
version = "0.1.1",
scripts = {
  test = m#"test.sh --option --install example --version "0.1.1""#m,
```

Apart from aesthetics, a more serious issue is inconsistency. If you bump the
version number in version, you may forget to do so in the test scripts as well,
leading to an incorrect configuration. To remedy this problem, let us have a
single source of truth by reusing the value of name and version in test, using
the interpolation syntax `#{expr}`:

```nickel
name = "example",
version = "0.1.1",
scripts = {
  test = m#"test.sh --option --install #{name} --version "#{version}""#m
```

Now, if we change version to "0.1.2" and export the result, the test script
invocation is updated as well:

```yaml
# [...]
scripts:
  do_stuff: do_stuff.sh subcommand
  test: "test.sh --option --install example --version \"0.1.2\""
  version: 0.1.2
```

## Going further

This was a short introduction that should get you started. But Nickel is a
full-fledged programming language, featuring higher-order functions, gradual
typing, contracts, and much more. You'll find more resources on the
[Documentation](/documentation) page.
