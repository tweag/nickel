---
slug: cookbook
---

# Cookbook

For now, this is an unorganized, temporary document not to forget some pieces of
the user manual that are not yet put in the right place.

## Library (record of functions)

You should use **type annotations** for records of functions. Currently Nickel
doesn't have a specific notion of a library or a module. You can just put
functions inside a record. In accordance with the previous section, you should
also use a type annotation on your record to make the type of the functions
accessible to the outside. Otherwise, the record is typed as `Dyn` and will
obliterate the types, making your library basically unusable inside typed code.

### Example

DON'T

```nickel
{
  foo :  Num -> Num = fun x => x + 1,
  bar : Num -> Num = foo,
}
```

BUT DO

```nickel
{
  foo = fun x => x + 1,
  bar = foo,
} : {
  foo : Num -> Num,
  bar : Num -> Num,
}
```

Alternatively, you can repeat your types both at the function level and at the
record level. It makes code more navigable and `query`-friendly, but at the
expense of repetition and duplicated contract checks. It is also currently
required for polymorphic functions because of [the following
bug](https://github.com/tweag/nickel/issues/360).

<!-- # Note: we already have type wildcard, but they don't "export" the inferred type. -->
<!-- A better solution will probably be implemented in the future: type wildcard (TODO: -->
<!--  -->
<!-- ```nickel -->
<!-- { -->
<!--   foo : Num -> Num = fun x => x + 1, -->
<!--   bar : Num -> Num = foo, -->
<!-- } : _ -->
<!-- ``` -->
