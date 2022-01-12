---
feature: merge types and terms syntax
start-date: 2022-01-12
author: Yann Hamdaoui
---

# Merge types and terms syntax

The present RFC aims to make the experience of writing Nickel -- and in
particular writing contracts -- more streamlined by using a common syntax for
terms and types.

In a standard statically typed language, terms and types are usually two
distinct syntactic categories. Most of the constructs on one side indeed don't
make sense on the other side: what could mean the type `1 + 1`, or the term
`forall a. a -> a`? Separating the two syntactically also allows for a better
mental separation for the user as well. Finally, separating the two gives more
room in the grammar of the language to reuse syntax in a different way. For
example in Nickel we are using the record syntax for both record literals et
record types, and we are using the pipe operator `|` both for metavalues (terms)
and row tails (types).

However, Nickel is different from the standard typed functional language.
Because of the duo static typing/contracts, we do have to:

- Give a static meaning to terms. This is the `#` operator, lifting a contracts
  (a term) to a type. Currently, this type is quite rigid: this is an opaque
  type.
- Give a dynamic meaning to types. Because each type annotation gives rise
  to a contract, we have to derive a contract from a type. There is currently no
  first-class operator to do that in Nickel, but the interpreter does it
  internally in the `open_contract()` function. Throughout this document, we
  will use the `§` operator to denote this operation, pretending it exists in
  the language.

We could precisely give a meaning to a type `1 + 1` (`#(1 + 1)`) and to a term
`forall a. a -> a` (`§(forall a. a -> a)`). Why we propose to do so is explained
in the next section.

## Motivation

The original motivation is that, when writing contracts, we may want to mix both
user-defined contracts as well as contracts derived from builtin types. A very
simple example is the following nullable contract, that takes another contract
as a parameter:

```nickel
let Nullable = fun contract label value =>
  if value == null then null
  else contracts.apply contract label value
in
...
```

Now, say we want to encode a nullable list:

```nickel
{
  list_maybe | #(Nullable §(List Num))
}
```

This is already not looking great, but bearable. However, mixing types and
contract even further is not improbable. For example, if we want to express that
a field is either `null` or a list of ports:

```nickel
{
  list_ports_maybe | #(Nullabe §(List #Port))
}
```

This starts to be really verbose and hard to parse. It's not only aesthetic:
when peer-programming Nickel with other Nickel beginners, they happened to be
confused as why sometimes we need a hash and sometimes not, why we used `List`
and sometimes `list` (the equivalent of `§` list in this document).

What's more, the type application syntax (`List Num`) actually coincides to the
standard application within contracts. That is, `§(List Num) ~ §List §Num`. It
would thus make sense to define type application for user-defined contracts by
`#Foo Type ~ #(Foo §Type)`, and to be able to write directly `#Nullable Num` or
`#Nullable #Port` instead of `#(Nullable §Num)` and `#(Nullable Port)`
respectively.
