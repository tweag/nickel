---
slug: tutorial
---

# Managing users

In this tutorial, you will learn how use Nickel to manage a list of users,
and then export it as a YAML file for a program consuming it.

## Step 1: Install nickel

The first step is to obtain nickel on your system. There are different
methods to install Nickel, they are all covered on the [Getting
started](https://nickel-lang.org/getting-started/#getting-started)
page of Nickel project website.

## Step 2: Think about the schema

The YAML we want to produce as different fiels with different types
(lists, booleans, strings), here is a sample of what we need:

```yaml
users:
  - name: Aisha
    is-admin: true
    ssh-keys:
      - AAAAApqCA8oKAB5S/47f...... aisha@work
  - name: Violet
    extra-groups:
      - accounting
```

We can spot attributes such as `name`, `ssh-keys`, `is-admin` or
`extra-groups`. To create a Nickel contract that correctly specifies
the shape of this data, we need to think about the type needed for
each attribute.

For example, the attribute `name` is a string, which translates as `Str`
in Nickel. Meanwhile, `ssh-keys` must allow multiple keys, so this is
an list of strings, written as `Array Str` in Nickel. `is-admin` is a
boolean, written as `Bool`. Finally, `extra-groups` is a list of group
names, which is the same type used for `ssh-keys`, we need `Array Str`.

We can also mark fields as optional so you won't have to explicitly write
about them if they don't have any value, `extra-groups` and `ssh-keys`
can be empty in the example, we will make them optional by using the
`optional` keyword.

The field `is-admin` must always be present in the YAML file, but
for most of our users it will be set to `false`. Fortunately, we can
assign the default value `false` to this field , which means you only
need write `is-admin = true` in Nickel code when the user is actually
an administrator.

## Step 3: Write a contract

```nickel
{
  UserSchema =
  {
    name | Str,
    ssh-keys | Array Str | optional,
    is-admin | Bool | default = false,
    extra-groups | Array Str | optional,
  },
}
```

## Step 4: Write users

```nickel
let {UserSchema, ..} = import "users-schema.ncl" in
{
  users | Array UserSchema =
  [
    {
      name = "Alice",
      is-admin = true,
      extra-groups = [ "support"],
      ssh-keys = [
        "AAAAAe4QAAAAAB5OLAo1...... alice@nixos",
        "AAAAA26vx+AqGIPZd/NT...... alice@android",
      ],
    },

    {
      name = "Bob",
      extra-groups = [ "pentester"],
    },

    {
      name = "Mallory"
    },

    {
      name = "Grace",
      is-admin = true,
      extra-groups = [ "administrative" ],
    },
 ]
}
```

## Step 5: Export as YAML

By default, nickel exports as JSON type, so we need to use the extra
parameter `--format yaml` to export as YAML.

```shell
nickel -f users.ncl export --export yaml
```

gives this result:

```yaml
---
users:
  - extra-groups:
      - support
    is-admin: true
    name: Alice
    ssh-keys:
      - AAAAAe4QAAAAAB5OLAo1...... alice@nixos
      - AAAAA26vx+AqGIPZd/NT...... alice@android
  - extra-groups:
      - pentester
    name: Bob
  - name: Mallory
  - extra-groups:
      - administrative
    is-admin: true
    name: Grace
```
