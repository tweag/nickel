# Representing data

Nickel's basic data types are [numbers](numbers.md),
[booleans](booleans.md), [strings](strings.md), [arrays](arrays.md),
and [records](records.md). All of these basic data types can be written
to and read from the analogous types in JSON, YAML, and TOML.

TODO: link to serialization

Nickel also supports data-bearing [enums](enums.md), also known as
"tagged unions" or "sum types". These can be used for programming in
Nickel, but are not interoperable with JSON, YAML, or TOML.
