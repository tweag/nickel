---
slug: tutorial
---

# Managing users

In this tutorial, you will learn how to use Nickel to manage a list of users and
to export it as a YAML file.

## Step 1: Install nickel

The first step is to make Nickel available on your system. Please refer
to the different installation methods documented in
[Getting started](https://nickel-lang.org/getting-started/#getting-started).

## Step 2: Think about the schema

The YAML we want to produce has different fields with different types (lists,
booleans and strings). Here is a sample of what we would like:

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

We can spot fields such as `name`, `ssh-keys`, `is-admin` or
`extra-groups`. To create a Nickel contract that correctly specifies
the shape of this data, we need to think about the type needed for
each field.

For example, the field `name` is a string, which translates to `String` in
Nickel. Meanwhile, `ssh-keys` must allow multiple keys, so this is a list of
strings, written as `Array String`. The field `is-admin` is a boolean, written
as `Bool`. Finally, `extra-groups` is a list of group names: we'll use `Array
String` here as well.

We can also mark fields as optional so we won't have to necessarily provide a
value. In our working example, `extra-groups` and `ssh-keys` can be empty, so we
will mark them as optional using the `optional` keyword.

The field `is-admin` must always be present in the YAML file, but for most of
our users it will be set to `false`. Fortunately, we can assign the default
value `false` to this field, which means we only need to write `is-admin = true`
when the user is actually an administrator.

## Step 3: Write a contract

Create a text file named `users-schema.ncl`. We will write our contract
defining fields and associated constraints such as a type, marking
the field optional, and a default value if there is one.

Please note that using a contract isn't mandatory per-se to use Nickel, but it
will allow you to validate your configuration (See [Step
6](#step-6-try-to-make-a-mistake) for an example), provide in-code information
thanks to the LSP, and to document your code at the same time.

```nickel
{
  UserSchema =
  {
    name
      | String,
    ssh-keys
      | Array String
      | optional,
    is-admin
      | Bool
      | default
      = false,
    extra-groups
      | Array String
      | optional,
  },
}
```

## Step 4: Write users

Now, create a text file named `users.ncl` that will import our contract
file created previously. This file will contain the actual data we need
to use.

```nickel #parse
let { UserSchema, .. } = import "users-schema.ncl" in
{
  users | Array UserSchema = [
    {
      name = "Alice",
      is-admin = true,
      extra-groups = ["support"],
      ssh-keys = [
        "AAAAAe4QAAAAAB5OLAo1...... alice@nixos",
        "AAAAA26vx+AqGIPZd/NT...... alice@android",
      ],
    },
    {
      name = "Bob",
      extra-groups = ["pentester"],
    },
    {
      name = "Mallory"
    },
    {
      name = "Grace",
      is-admin = true,
      extra-groups = ["administrative"],
    },
  ]
}
```

## Step 5: Export as YAML

By default, nickel exports data as JSON. But we can change that using
the extra parameter `--format yaml` to export as YAML.

```shell
nickel export --format yaml users.ncl
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

## Step 6: Try to make a mistake

In this extra step, we will make a mistake on purpose in the file
`users.ncl` and try to export the data as YAML. We will see what happens
when Nickel detects that a value doesn't satisfy a contract.

Edit the file `users.ncl` and delete the line `name = "Alice",`. Now
export the file again using `nickel export --format yaml users.ncl`.
You should see the following error:

```console
$ nickel export --format yaml users.ncl
error: missing definition for `name`
   ┌─ users-schemas.ncl:4:5
   │
 4 │     name
   │     ^^^^ defined here
   │
   ┌─ users.ncl:4:5
   │
 4 │ ╭     {
 5 │ │       is-admin = true,
 6 │ │       extra-groups = ["support"],
 7 │ │       ssh-keys = [
   · │
10 │ │       ],
11 │ │     },
   │ ╰─────^ in this record
```

The first part tells us where `name` was defined as being a requirement. This
points to the definition of name inside `UserSchema`. As there is no `optional`
keyword, this field must be set in the final configuration.

The second part tells us that in the first record in the users list, the field
`name` has no value while it should have one. This is to be expected as we
removed it earlier.
