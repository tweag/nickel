# LSP Semantics

These are work-in-progress notes on how we want the language server to behave.

## Goto definition

The LSP has support for going to declarations, definitions, type definitions, and implementations. It
also has a "find references" method, which is sort of like a goto.

Here is the easy base for goto definition: when a variable is bound by a
`let`, `fun`, or `match`, the variable binding (possibly in a pattern) is an LSP
"definition" and any uses of that variable are LSP "references".

For example, in `let foo = 3 in 4 + foo` a "list references" request on the
first `foo` should return the second `foo`, and a "goto definition" on the
second `foo` should return the first `foo`.

Static accesses are a little more challenging: given a path like `foo.bar.baz`
and a "goto definition" request for `baz`, it would be nice if we could find
where the value for `baz` is set (i.e. some part of the code that looks like
`{ ..., baz = 2, ...}`). This cannot be done in general (at least, without
evaluation) because `foo` could be the result of some arbitrary computation.
Nevertheless, we make some effort at static analysis for the common cases.

Let's describe a few simple cases first:

- The fields in a record literal
  are LSP "definitions" and any static accesses of them are LSP "references".
  For example, in `{bar = 3}.bar` the first occurrence of `bar` is
  a definition and the second is a reference to it.

- For all path elements except the last, `let` bindings should be transparent.
  For example, in each of

    - `let foo = { bar = 3 } in foo.bar`,
    - `let baz = { bar = 3 } in let foo = baz in foo.bar`, and

  the second occurrence of `bar` references the first. 

- The definitions should be repeatedly "resolved" for all path elements
  except the last.
  In `let foo = { baz = { bar = 3 } } in foo.baz.bar`, the second occurrence
  of `bar` references the first.

- In the merge of two records, fields defined on both sides are definitions.
  We don't take merge priorities into account, because it's probably useful for the user to
  see the overridden values in addition to the values that "win." Note that the LSP allows
  multiple responses to a "goto definition" request.

  For example, in `let x = { foo | default = 3, bar = 4 } & { foo = 2 } in [x.foo, x.bar]`
  the second instance of `bar` references the first instance, while the last instance of `foo`
  references the first two instances.

- If-then-else acts like a merge: both branches provide definitions, so that
  in `let x = if ... then { foo = 1 } else { foo = 2 } in x.foo`, the last instance of `foo`
  references the first two instances.

- This might be getting too fancy, but we could potentially do static analysis of functions
  to track the origin of record fields. For example, we might expect that sprinkling the
  identity function around doesn't affect definition tracking.

### The typescript-language-server

`typescript-language-server` has a somewhat similar behavior to what we're aiming for, and
it can even "see through" functions to some extent.
For example, in

```typescript
function foo() {
  return { foo : 1 };
}

const x = foo();
x.foo;
```

then "goto definition" on the final `foo` points to the `foo` in the record literal.
The algorithm seems to be mainly type-directed. For example, annotating `foo()`
as `function foo(): any { ... }` breaks going to the definition of `foo`

### Proposed algorithm

The core part of the algorithm is a function mapping terms to sets of
record literals, giving for each term the set of record literals that might
"contribute" (through, e.g., merging) to that term. This will only be an
approximation, of course, because we aren't evaluating. Let's call this process
"record literal resolution," and it is defined by:

- a record literal resolves to itself
- a variable resolves to the term that it's bound to
- a merge resolves to the union of the resolutions of the two merged terms
- an if-then-else resolves to the union of the resolutions of the two branches
- everything else resolves to the empty set (but this can be extended later)

Now to find the definition of `baz` in `foo.bar.baz`, we first resolve `foo`
to a set of record literals. For each of those literals containing a `bar` field,
we resolve their values to a set of record literals. For each of those literals
containing a `baz` field, we report it as a definition of `baz`.

## Goto type definition

Since we cannot (yet) name custom types, it probably doesn't make sense to use "goto type definition"
for static types. Instead, we could use the LSP "goto type definition" request for going
to contract definitions: given `x | Foo`, a "goto type definition" request for `x` should be the same
as a "goto definition" request for `Foo`. This would respect record contracts, so that
in `{x = 1, y = 2} | { x | Foo, y | Bar }` a "goto type definition" request for `x` should again
be the same as a "goto definition" request for `Foo`.

## Completion

Here is a list of items that we might want to provide completions for:

- record fields, as in `let x = { foo = 1 } in x.fo`
- enum variants
- variables in scope, as in `let foo = 1 in 2 + fo`
- filenames in imports, as in `import "fo` when `foo.ncl` exists on disk
- maybe keywords? They're pretty short in nickel

One of the trickier parts of handling completion is that the input will be incomplete
and may not parse. Let's ignore that for now, and assume that we have a full AST.

In LSP, the editor (and not the language server) is in charge of text-based filtering.
That is, if the user enters `2 + fo` and requests completion, we can return all of the
names of variables in scope; the editor is in charge of filtering out all those that
don't start with "fo". When types are involved, the responsibilities swap: when
completing `x.fo` the language server should return only the field names belonging
to `x` (it still doesn't need to care about whether they start with "fo").
Type information can also be used to filter completions for enum variants and
variables in scope (more on that below).

In summary, the desired completion behavior is:

- when completing a record path, find the type (or contract) of the second-last
  element of the path. If it's a record type, return all of its fields.
- when completing an enum variant, if we know the type of the term that's being
  completed (and it's an enum type), return all of that type's known enum variants.
  Otherwise, just return all the enum variants we've seen ever.
- when completing a variable in scope, if we know the type of the term that's being
  completed then return all of the in-scope variables that are either `Dyn` or have
  the right type. Otherwise, just return all the variables in scope.
- when completing an import filename, we could return all files in the directory tree
  and let the editor sort them out, but this is probably slow. Instead, take the
  basename of the path so far and (if it points to a directory that exists) return
  all files in that directory
- if we decide we want to complete keywords, just return all the keywords and let
  the editor filter them