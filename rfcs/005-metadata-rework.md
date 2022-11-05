---
feature: representation and semantics of metadata
start-date: 2022-09-07
author: Yann Hamdaoui
---

# The representation and semantics of metadata

The goal of this RFC is to rethink how metadata (documentation, priority, custom
merge function, etc.) are represented, accessed, and propagated, and what is
their semantics with respect to merging.

## Motivation

This RFC has two main motivations:

1. **Merge semantics**: Fix a number of strange and unintuitive behaviors
   observed in the wild while prototyping a Nix package set written in Nickel. A
   detailed report is given in
   [#710](https://github.com/tweag/nickel/issues/710). Most of the examples and
   explanations from this report are reproduced here for this RFC to be
   self-contained.
2. **Representation**: Simplify the implementation and make it more efficient.
   The current representation of metadata implies that fetching the metadata of
   a value may involve evaluating arbitrary Nickel expressions, which doesn't
   really fit well with the idea of metadata. One striking example is optional
   fields, which is detailed later in this document.

### Merge semantics

This section explores the first motivation: the current semantics of merging
metadata is fragile.

The idea of merging is mainly inspired from two models[^1], the [NixOS module
system](https://nixos.wiki/wiki/Module) and the [CUE programming
language](https://cuelang.org/docs/concepts/logic/).

The NixOS module system and the CUE merge system are fundamentally based on the
idea of composing small logical data blocks into a final result. Those blocks
are not necessarily independent: a value defined in a block may depend on other
values that will only be available later in the big merged blob (for NixOS
modules, those are typically options). In Nickel terms, this is often referred
to as _recursive overriding_.

This capability is what makes merging powerful, but comes with its share of
questions. What should be the result of the following program?

```nickel
{
  foo | Num
      | default = 5,
  bar = foo,
} & { foo = "a" }
```

If anything, this expression should evaluate to `{foo = "a", bar = "a"}`.
However, the fact that `foo` has been annotated with a `Num` contract but may
end up with a string in the final result without an error is disturbing. This
example is small and artificial, but in real life the two merged blocks could be
defined in different files, and the reason as to why an `"a"` ends up here where
a number was expected could be hard to investigate.

In CUE, the underlying model guides the answer to this question. Types and
values are not distinct entities (roughly, values can be seen as types with only
one inhabitant, i.e. singletons), but live all in the same space. Merging
intuitively corresponds to intersection. In that interpretation, trying to merge
`"a"` with something of type `Num` will throw an error. Similarly, in NixOS
modules, a configuration option of type string won't be merged with a number
without an error (Nix doesn't have builtin types per se, but the NixOS module
system is a library with a notion of type similar in spirit to Nickel's
contracts).

Thus, it seems reasonable to do the same in Nickel. Let us write a first
specification that we would like our system to enjoy:

**Eventual validity `(EvVal)`**: let `e` be a merge expression
`x_1 & ... & x_n`, where each `x_i`s evaluate to a record. Then, if there exists
`j` such that `x_j` has a field `f` with a contract `C` attached, then `e.f | C`
(`e.f` respects the contract `C`)

#### Cross-application

Nickel does respect `(EvVal)`. Actually, it does something stricter in the sense
that more expressions are blamed than what would be strictly required by
`(EvVal)`. The current implementation eagerly "cross-applies" contracts of both
sides:

**Cross application `(CrossApp)`**: let `e` be a merge expression `x & y`, where
`x` and `y` evaluate to records. Let `f` be a field, `A1, ..., An` be the
contracts attached to `f` in `x`, and `B1, ..., Bm` the ones attached to `f` in
`y`, then:

```text
e.f = (x.f | B1 | ... | Bm) & (y.f | A1 | ... | An)
```

(with the obvious extensions to edge cases where `f` is not defined in `x` or
`y`, or where either `n` or `m` is zero)

Note that in the current implementation, `A1, ..., An` have already been applied
to the value of `x.f`. So, if we repeat all the contract applications that end
up being evaluated, we get:

```text
(x.f' | A1 | ... | An | B1 | ... | Bm) & (y.f' | A1 | ... | An | B1 | ... | Bm)
```

Where `x.f'` and `y.f'` are the original definitions for `f` before any contract
application.

#### Cross-application is too eager

This choice has a number of unintuitive consequences. As opposed to the desired
specification `(EvVal)`, the implementation `(CrossApp)` may require
intermediate values (that are built up during merging) to respect all the
contracts attached to a field, while `(EvVal)` only requires that the _final_
value respect all the contracts.

For example, contracts derived from record types are immediately testing for the
absence and the presence of fields. Thus, the following program currently fails:

```text
nickel>{
  foo | {
    bar : Num,
    baz : Str
  }
}
& {foo = {}}
& {foo.bar = 1}
& {foo.baz = "a"}

error: contract broken by a value: missing field `bar`
  ┌─ :1:1
  │
1 │ {bar: Num, baz: Str}
  │ -------------------- expected type
  │
  ┌─ repl-input-8:7:10
  │
7 │ & {foo = {}}
  │          -- evaluated to this expression
  │
  ┌─ <unknown> (generated by evaluation):1:1
  │
1 │ { ... }
  │ ------- evaluated to this value

note:
  ┌─ repl-input-8:2:9
  │
2 │     foo | {
  │ ╭─────────^
3 │ │     bar : Num,
4 │ │     baz : Str
5 │ │   }
  │ ╰───^ bound here
```

Another eager contract is the dictionary contract `{_ : T}`. The following
example (simplified) has been naturally written while brainstorming on how a Nix
package would look like in Nickel:

```nickel
nickel>let Drv = { out_path | Str, ..} in
let Package = { name | Str, drv | Drv, .. } in
{
  build_inputs | {_: Package} = {
    foo,
    bar,
  },
  build = m%"
    %{build_inputs.foo.drv.out_path}/bin/foo $out
  "%m,
} & {
  build_inputs = {
    foo = { name = "foo", drv.out_path = "/fake/path" },
    bar = { name = "bar", drv.out_path = "/fake/path" },
  }
}

error: missing definition for `bar`
```

Surely, `bar` is not missing a definition in the final combined record. The
issue is that the implementation of the `{_: T}` contract maps `T` onto the
fields of its argument. This operation is eager, in that if one field doesn't
have a definition right away, this operation fails. Because we do `(CrossApp)`, the
`{_ : Package}` contract is applied independently to both `{foo, bar}` and `{foo
= ..., bar = ...}`. The former, which serves as an interface, is missing
definitions and fails the contract no matter what it will be merged with.

For this particular case, we could make the `{_: T}` contract to map lazily, but
that would require a new dedicated operator. And this sounds like yet another
special casing: fact is, similar variants of record contracts could very well be
user-defined, and we can't special case them all.

#### Cross-application breaks associativity

Recall the first failing example of the previous section:

```text
nickel>{
  foo | {
    bar : Num,
    baz : Str
  }
}
& {foo = {}}
& {foo.bar = 1}
& {foo.baz = "a"}

error: contract broken by a value: missing field `bar`
```

Now, if we just add parentheses to first merge the concrete pieces of data
together, the contract will be applied on the fully formed record, and the new
program succeeds:

```text
nickel>{
  foo | {
    bar : Num,
    baz : Str
  }
}
& ({foo = {}}
& {foo.bar = 1}
& {foo.baz = "a"})

{ foo = { baz = "a", bar = 1 } }
```

Thus, `(CrossApp)` breaks associativity of merging, which is a desirable
algebraic property, and which is otherwise valid.

#### Outer cross-application doesn't make sense

Another source of strange behavior is when merging records with contracts
attached (the contracts aren't attached to a field, but to the whole record).
For example, what is the result of the following program?

```nickel
({foo = 5} | {foo | Num})
& {bar = "bar"}
```

We merge two records, and one is additionally checked by a contract. Everything
looks fine, and we expect this program to evaluate to `{foo = 5, bar = "bar"}`:

```text
nickel>({foo = 5} | {foo | Num})
& {bar = "bar"}

error: contract broken by a value: extra field `bar`
  ┌─ :1:1
  │
1 │ { ... }
  │ ------- expected type
  │
  ┌─ repl-input-1:2:3
  │
2 │ & {bar = "bar"}
  │   ^^^^^^^^^^^^^ applied to this expression
  │
  ┌─ <unknown> (generated by evaluation):1:1
  │
1 │ { ... }
  │ ------- evaluated to this value

note:
  ┌─ repl-input-1:1:14
  │
1 │ ({foo = 5} | {foo | Num})
  │              ^^^^^^^^^^^ bound here
```

Ouch. What happens is that our semantics applies to top-level merge expressions,
not only to record fields. In consequence, the `{foo | Num}` contract is
cross-applied to `{bar = "bar"}` value.

Indeed, we only have one merge operator, and we defined quite straightforwardly
the merging of records to be recursive: `{foo = exp1, ...} & {foo = exp2, ...}`
evaluates to `{foo = exp1 & exp2, ...}`. Thus, we currently can't make a
difference between "I am merging the fields of record as a result of a prior merging
of those records" from "I am merging two top-level values", and contract are
propagated in both cases.

This isn't related to `(CrossApp)` vs `(EvVal)`: any `(EvVal)`-compliant implementation
could exhibit this behavior. The issue appears because we have a simple
recursive definition of merging for record fields that doesn't differentiate
between a top-level merging and an "inherited" merging `&'`, such that `{foo =
exp1} & {foo = exp2}` is `{foo = exp1 &' exp2}`, and where `&'` may have a
different behavior with respect to contract applications.

### Representation

This section details the second motivation: the current representation of
metadata in the AST makes them hard to fetch.

Metadata (originally called _enriched values_) have been implemented as a
special node in the AST which holds attributes in addition to a standard value.
The advantages of having a builtin notion of metadata – as opposed to simply
represent them explicitly as a record in Nickel, e.g. `{meta: Metadata, value:
Dyn}` – is to make them mostly transparent for the rest of the language. More
precisely:

1. Metadata are transparent for evaluation. The interpreter automatically
   unwraps the inner value silently by default. If we were to use an explicit
   representation as a record, now operators like `+` would have to work both on
   numbers and on values of type `{meta: Metadata, value: Num}`. Ditto for
   basically all builtin operators.

   Currently, only specific operations, like merge or fetching metadata, need to
   be metadata-aware. They do so thanks to a special mode in the evaluator, and
   the rest of the evaluation can just pretend they don't exist.

2. Similarly, metadata are transparent for typechecking. Attaching a
   documentation to a value doesn't change its type (while, morally, we are
   turning a value of type `a` to `Meta a = {meta: {..}, value: a}`).

With the current design, _metadata can basically appear anywhere in the AST_. As
a positive consequence, they propagate partly throughout computations, as long
as a value with metadata is not forced. For example:

```text
nickel> let some_block = {
  field | Num
        | doc "Some field"
        | default = 5,
  field_array = [field, 0],
}
nickel> let other_block = {
  field_array,
  field_head = array.head field_array
} & some_block
nickel>:query other_block.field_head
• contract: Num
• default: 5
• documentation: Some field

```

Here, the metadata of `field` have propagated through to the array element
defined in `field_array`, that is then extracted in the field `field_head` of
`other_block`.

However, having metadata everywhere has also a cost.

#### Fetching metadata

The first one is that the operation of fetching metadata involves evaluating
arbitrary Nickel expressions. In the last example, querying
`other_block.field_head` is done by evaluating `other_block.field_head` in
so-called non-strict mode. This is like normal evaluation, but stops as soon as we
encounter metadata.

This evaluation is required because when we get back an expression for
`field_head`, the interpreter has absolutely no way of knowing in advance if
metadata are hiding somewhere, and how much evaluation is required to extract
them. We have no option but to evaluate the term until we reach either a value
or a metadata node. If there is no metadata in the end, we've done all the work
for nothing.

For querying metadata through `nickel query`, this might be fine: in fact,
`query` also displays the value of the field, which requires evaluation anyway.

But in other cases, this cost may be prohibitive. On such example is optional
fields (see [#815](https://github.com/tweag/nickel/pull/815)). It seems natural
to store them as metadata (same as for default values). But then, record
operations such as `record.fields` need to decide "is this field optional and
empty", and a linear time operation suddenly becomes arbitrary long as it might
imply to evaluate each and every field.

#### Nested metadata

The second issue with metadata anywhere is the semantics of nested metavalues.
Currently, when fetching, we stop at the first metadata we encounter. But they
may very well be nested. The current implementation is oblivious to potential
inner metadata:

```text
nickel>let base = {
  foo | doc "Foo"
      | Str
      | default = "bar"
}
nickel>:query base.foo
• contract: Str
• default: "bar"
• documentation: Foo

nickel>let base_hidden = {
  foo = ((("bar" | doc "Foo") | default) | Str)
}
nickel>:query base_hidden.foo
• contract: Str
• value: "bar"

```

One solution could be to define a flattening operation (pedantically, a monoid
operation, or a monad if metadata are rather seen as a container over a value):
`flatten : (Metadata, Metadata) -> Metadata`.

While fetching metadata can take an arbitrary time to complete _in principle_,
it's currently quick for the happy path, when metadata are located e.g. directly
on record fields. Now, if we want to correctly handle nested metadata, we can't
stop evaluation as soon as we encounter metadata anymore, because some other
metadata could be lying underneath. We now have to _systematically evaluate an
expression down to a value_, collecting and flattening metadata along the way.

## Proposal

In a configuration language such as Nickel, the fundamental data structure is
the record, and the fundamental piece of data is the record field. In
retrospect, fields seem to be the right level of granularity for metadata, and
where they are most often attached in practice.

While metadata that can appear anywhere are in theory more flexible, the only
example making use of such capability of metadata to be attached to other values
than record fields is the example where metadata are propagated through an array
and another record field. This example is contrived, and doesn't seem to
correspond to a real use-case in a convincing way. In the NixOS module system,
for example, metadata are attached to options only.

Field-based metadata would only be affected during merging, and combined eagerly
when that happens: no later arbitrary evaluation, no nesting, just static data.
Allowing them anywhere doesn't bring clear practical benefits, whereas it comes
with substantial drawbacks, exposed in the previous sections.

### Specification

The proposed change comes in to aspects:

1. Metadata can only be written next to record fields, and are stored only there
   in the AST. Their semantics with respect to merging is left unchanged, except
   for cross-application of contracts (see the next point). Contract annotations
   and type annotations are still allowed on any value, but would be parsed as a
   different node (say, `Annotated`) and wouldn't impact the behavior of
   merging.
2. Instead of implementing `(CrossApp)`, we implement a laxer strategy,
   lazy propagation (which corresponds closely to `(EvVal)`). As we've done with
   [lazy array contracts](https://github.com/tweag/nickel/pull/764), contracts
   would be accumulated at the field level instead of being eagerly applied.
   Only when the field is extracted (or evaluated by any other mean: equality
   testing, deep sequing, etc.), we can apply the pending contracts. As for lazy
   array contracts, this also enables new potential optimizations, like
   eliminating redundant contract applications.

In 2., note that we just apply the accumulated contracts in sequence. In some
sense, we form the conjunction of all the contracts, or equivalently the
intersection. This intersection is however _naive_, so to speak. This
intersection works well for common data contracts (predicates on numbers and
strings, record and array contracts, etc.) where it corresponds to the intuition
for the boolean _AND_., but not so much for function contracts. For example,
`({f | Num -> Num} & {f | Str -> Str} & {f = fun x => x}).f "a"` will blame,
while `f = fun x => x` does satisfy each function contract in isolation. For
more details, see [Union and Intersection Contracts are Hard,
Actually](https://dl.acm.org/doi/10.1145/3486602.3486767).

### Revisiting the initial issues

- **cross-application is too eager**: this problem is solved by the
  implementation of lazy propagation.
- **outer cross-application doesn't make sense**: this is solved (somehow for
  free) by the restriction to record fields. Recall the original example:

    ```nickel
    ({foo = 5} | {foo | Num})
    & {bar = "bar"}
    ```

  Now, because the annotation `| {foo | Num}` is not attached to a field, this is
  not a metadata, but a mere annotation, and doesn't impact merging. In
  particular, it doesn't propagate to the right-hand side `{bar = "bar"}`, which
  is the desired outcome.
- **fetching metadata**: now metadata are only stored at record fields. We will
  probably have to restrict querying to record fields, but it is then accessed
  directly from a record.
- **nested metadata**: nested metadata are still possible, in a way. For
    example:

    ```nickel
    {
      foo | doc "Foo" = bar.baz,
      bar.baz | doc "Baz" = null,
    }
    ```

  We would still only return the outer metadata attached to `foo`. However, this
  semantics is arguably more natural here, because the `"Baz"` documentation is
  attached to a different field, even if `foo` inherited the same value. The
  right documentation to report seems to still be the one of `foo`.

  While in the previous situation, in a case like `{ foo = ((function.id (null |
  default)) | doc "doc") }`, ignoring the inner annotation (`default`) is
  arguably harder to justify.

### Additional consequences

#### Propagating metadata

The field-based approach seems more intuitive in other situations. The current
propagation of metadata isn't always the right semantics:

```text
nickel>let config = {
  foo = bar,
  bar | default = 5,
}
nickel>config & {foo = 2}
{ bar = <default=5>, foo = <value=2> }

```

Here, we are able to override `foo`, although it hasn't been declared as having
a default value. Because metadata propagate, `foo` inherits the default
attribute from `bar`. The fact that `foo` is a default value should rather
depend on `foo` being annotated with `default`, and shouldn't necessarily
depend transitively on its assigned value. For example, if we override `bar` with
a definite value in between, now `foo` can't be overridden anymore:

```text
nickel>let config = {
  foo = bar,
  bar | default = 5,
}
nickel>config & {bar = 3} & {foo = 2}
error: non mergeable terms
  ┌─ repl-input-4:1:17
  │
1 │ config & {bar = 3} & {foo = 2}
  │                 ^           ^ with this expression
  │                 │
  │                 cannot merge this expression

```

On the other hand, with a field-based approach, `foo` wouldn't inherit any
metadata from `bar`, and wouldn't be a default value (both examples would fail
consistently with a non mergeable terms error).

#### Missing definition

Currently, reporting an error for missing the definition of a field is quite
complicated. By the dynamic nature of metadata, once we try to eventually
evaluate metadata with no inner value, we are missing the original context: just
getting the name of the field that we are currently evaluating requires to
maintain an additional call-stack, for the sake of error reporting only, and to
use heuristics to try to extract meaningful information.

As of today, this is still not totally reliable: here is an example (on recent
master, 4bba9f0481d6e050b6d488ea0ce83d9742490fa4) where the error reporting is
totally off:

```text
nickel>let r = {
  without_def,
  field_head = array.head without_def
}
nickel> :query r.field_head
error: missing definition for `head`
    ┌─ <stdlib/array>:2:11
    │
  2 │     array = {
    │ ╭───────────^
  3 │ │     NonEmpty
  4 │ │       | doc m%"
  5 │ │         Contract to ensure the given array is not empty.
    · │
273 │ │           (sort cmp (parts.right)) @ [first] @ (sort cmp (parts.wrong)),
274 │ │   }
    │ ╰───^ in this record
    │
    ┌─ repl-input-1:3:16
    │
  3 │   field_head = array.head without_def
    │                ---------- accessed here

```

Of course, `array.head` exists, and the issue is a missing definition for `r.without_def`.

If we store metadata at the field level, the only place where a value may miss a
definition (that is, where there is an optional in the implementation) is at
fields. When evaluating a field, we can now right away if it's missing a
definition, and throw an error with the required context. It's not entirely true
because of recursive fields, but it still potentially simplifies error reporting
a lot, as compared to the current situation where an empty metadata can occur at
any point during evaluation.

### Conditionally-defined fields

Conditionally-defined fields have been proposed in
[#459](https://github.com/tweag/nickel/issues/459). This feature exists in CUE,
and in the NixOS module system as `lib.mkIf`. Fields can then be defined
conditionally, depending on an arbitrary condition that possibly involve other
fields of the record. This is yet another feature that would fit in metadata,
but this is however quite hard to implement with the current metadata
representation, for reasons similar to optional fields.

As for optionals, a field-based representation makes the task much simpler, by
storing the conditional expressions directly at the record level. This
expression can be forced when requested by a record operation.

[^1]: (Interestingly, the [mixin
  modules](https://dl.acm.org/doi/abs/10.1145/97946.97982) of ML turns out to
  bear a lot of similarity with merging recursive records, but in retrospective
  only: both the original working draft of the merge system of Nickel and the
  NixOS module system were developed independently from mixin modules)
