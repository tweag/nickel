# Enums

An enum (short for "enumeration") is composed of a tag and an optional argument.
An enum without an argument is called an **enum tag**.
An enum tag with an argument is called an **enum variant**.
On its own, **enum** refers to either.

## Enum tags

Enumeration tags are used to express a choice among finitely many
alternatives. They are formed by writing a single quote `'` followed by
a valid identifier or a quoted string. For example, `'Json`,
`'Toml`, `'Yaml`, and `'"5x9"` are enum tags.

An enum tag `'foo` is serialized as the string `"foo"`:

``` { .nickel #repl }
> std.serialize 'Json {foo = 'bar}
"{\n  \"foo\": \"bar\"\n}"
```

While it's technically possible to just use strings in place of enum
tags, using an enum tag encodes the intent that only a finite number of
alternatives can be used for the corresponding value.

Additionally, using enum tags instead of strings allows for the contract
system (TODO: link) and the typechecker (TODO: link) to issue better
error messages when unexpected values are encountered.

## Enum variants

An enum variant is an enum tag with associated data.
It is formed by applying an enum tag to one argument: `'Foo 5`
represents the enum variant with tag `'Foo` and data `5`.

``` { .nickel #repl }
> 'Foo 5
'Foo 5

> 'Greeting ("Hello," ++ " world!")
'Greeting "Hello, world!"

> 'Operation { op_type = 'select, table = "users", clause = 'Where "id=1" }
'Operation { clause = 'Where "id=1", op_type = 'select, table = "users", }
```

A common pattern in Nickel uses enum variants to represent the result of
a function that may encounter a non-fatal error:

``` { .nickel #repl }
> let first_elem = fun array =>
   if array == [] then
     'Error "empty array"
   else
     'Ok (std.array.elem 0 array)
  in
  first_elem []
'Error "empty array"
```

Enum variants are the only primitive data structure of Nickel that can't
be serialized (TODO: link). Indeed, there is no obvious *canonical* way
to encode enum variants in the JSON data model (though many such
encodings exist). If you need to serialize and deserialize enum
variants, you'll have to explicitly map them to and from serializable
data structures (such as records).
