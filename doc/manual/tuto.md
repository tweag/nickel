---
slug: tutorial
---

# Managing users

In this tutorial, you will learn how use Nickel to manage a list of users,
and then export it as a YAML file for a program consuming it.

## Step 1: Install nickel

## Step 2: Think about the schema

Our current YAML has different fields, such as `name`, `ssh-keys`,
`is-admin` or `extra-groups`. To create a Nickel contract that produce
what we want, and validates input, we need to think about the type of
each field.

Nickel provides a lot of types, but also structures. In our case,
we want `ssh-keys` to allow multiple keys, so this will be an array,
`name` is a sequence of characters, so a type `Str` (for String),
the field `is-admin` can be either true or false, so this is a
`Bool`, finally, `extra-groups` is a list of group names,
which translates as an array of string.

We can also define some fields optionals, so you don't have
to fill them if they don't have any value, `extra-groups` and `ssh-keys`
can be empty in our case, we will make them optional by applying `|
optional`. The field `is-admin` must be provided to our program consuming
the YAML, but we don't want to fill it when it's false, we can apply
a default value of `false`, so we will only define it in our file when
the user is actually an admin, this makes the file maintenance easier.

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
