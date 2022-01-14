---
feature: merge types and terms syntax
start-date: 2022-01-12
author: Yann Hamdaoui
---

# Merge types and terms syntax

The present RFC aims to make the experience of writing Nickel -- and in
particular writing contracts -- more streamlined by using a common syntax for
terms and types. As it impacts the syntax, the plan is to have it implemented
before the first release. In consequence, we have often preferred
straightforward solutions that are less likely to delay said release, as long as
they are forward compatible. This is by no mean a final answer to all the
aspects covered in this document.

In a standard statically typed language (without dependent types), terms and
types are usually two distinct syntactic categories. Most of the constructs on
one side indeed don't make sense on the other side: what could mean the type
`1 + 1`, or the term `forall a. a -> a`? Separating terms and types
syntactically allows for a better mental separation for the user as well.
Finally, separating terms and types gives more room in designing the syntax of
the language to reuse symbols in a different way. For example, in Nickel, we are
using the pipe operator `|` both for metavalues (terms) and row tails (types).

However, because of the interaction between static typing and contracts, Nickel
is different from the standard typed functional language. We have to:

- Give a static meaning to terms. This is the `#` operator, lifting a contract
  (a term) to a type. Currently, this type is quite rigid: this is an opaque
  type.
- Give a dynamic meaning to types. Because each type annotation gives rise
  to a contract, we have to derive a contract from a type. There is no
  first-class operator to do that in the current syntax, but the interpreter
  does it internally via the `open_contract()` function. Throughout this
  document, we will use the `§` operator to denote this operation, pretending it
  exists in the language.

Thus, we can precisely give a meaning to a type `1 + 1` (`#(1 + 1)`) and to a
term such as `forall a. a -> a` (`§(forall a. a -> a)`). Why we propose to do so is
explained in the next section.

## Motivation

When writing contracts, we may want to mix both user-defined contracts as well
as contracts derived from builtin types. A simple example is the following
nullable contract, that takes another contract as a parameter:

```nickel
let Nullable = fun contract label value =>
  if value == null then null
  else contracts.apply contract label value
in
null | #Nullable
```

Now, say we want to encode a nullable list of numbers:

```nickel
{
  list_maybe | #(Nullable §(List Num))
}
```

This is already not looking great, but bearable. However, nesting types and
contracts even further is not out of the question. For example, if we want to
express that a field is either `null` or a list of ports:

```nickel
{
  list_ports_maybe | #(Nullabe §(List #Port))
}
```

This is verbose and hardly readable. The issue isn't aesthetic only: when
peer-programming Nickel with beginners, they happened to be confused as why
sometimes we need a hash and sometimes not, why we used `List` and sometimes
`list` (the equivalent of `§List` in this document), and so on.

It also turns out the type application syntax `List Num` (actually, there is no
such thing as general type application currently, but rather `List` is a
parametrized type with special treatment) coincides with the standard
application of contracts seen as terms, that is `§(List Num) = §List §Num`. It
would thus make sense to define type application for user-defined contracts by
`#Foo Type ~ #(Foo §Type)`, and to be able to write directly `#Nullable Num` or
`#Nullable #Port` instead of the oddly different forms `#(Nullable §Num)` and
`#(Nullable Port)` respectively.

This RFC proposes to make both terms and types to share the same syntax, such
that we could just write:

```nickel
{
  list_ports_maybe | Nullabe (List Port)
}
```

Which looks more natural all while having a reasonable semantics.

## Notations
We use the following notations throughout this document:

- `Term` is the subset of the current syntax that includes terms: records,
  lists, functions, let-binding, etc.
- `Type` is the subset of the current syntax that includes types: arrows, record
    types, enums, etc.
- `UniTerm` is the new unified syntax we are defining in this document.
- `# : Term -> Type` is the operator lifting a term to a type. It is part of the
    current syntax.
- `§ : Type -> Term` is the operator associating a term (the derived contract)
  to a type. It is not part of the current syntax, but exists implicitly as
  implemented by the interpreter's `Types::open_contract` function.
- `! : Type \/ Term -> UniTerm` is the embedding of the old syntax in the new
  one. It basically just erases the `#` and `§` and may replace symbols by
  others, as we will see, but is otherwise the identity function almost
  everywhere.

## Challenges

Such a change faces two main challenges:

### Semantics

The first challenge is to give a semantics for all the strange term we can now
write:

```nickel
let foo : forall a. a -> (fun x => x + 1) -> a + 1
```

Once again, we can rely on the fact that the current language already has the
generic transformations `# : Term -> Type` and `§ : Type -> Term`. Our goal is
to define a new syntax `UniTerm`, and two translations `type : UniTerm -> Type`
and `term : UniTerm -> Term`, based on `#` and `§`. We will then be able to
write Nickel programs in `UniTerm` and translate them to the current syntax.

### Conflicts

The second challenge is syntaxes clashes. Because the syntaxes were separate, we
use the same constructs for different objects in terms and in types. More
precisely, here is a list of the current conflicts:

- The pipe operator `|` is used for metavalue and contract annotation in terms,
  and for polymorphic record tails in types. For example, `{foo : Num | a}`
  could now be interpreted as a record with a field `foo` with a `Num`
  annotation, and a polymorphic tail `a`, or it could be interpreted as a
  field `foo` with two annotations, a type annotation `Num`, and a contract
  annotation `a` (that is, `{foo : Num | #a}` in the current syntax).
- The syntax for enum types `<foo, bar>` conflicts with the syntax of comparison
  operator. Typically, `a <foo> bar` can be parsed as `a §(<foo>) bar` or
  `(a < foo) > bar`.
- The syntax for record literals may conflict, as it is used both for record
  types and records themselves. Now, how should we understand `{foo : Bar, bar =
  2 | a}` as a type? And as a term?

## Proposal

### The `UniTerms` syntax

We define the `UniTerm` syntax as the union of the two currently separate
syntaxes, but with `#` removed (remember that `§` isn't currently actually in
the syntax, otherwise, we would remove it too).

#### Records

The current syntax already supports a term with lone type annotations such as
`{foo : Num}`, since the piecewise signature PR. Hence record types of `Type` is
almost already a subset of records of `Term`. Still, the following points need
clarification:

- **Conflict for `|`**: `|` is used both for tail and metavalue. Thus, `{foo :
  Num | a}` can be interpreted both as the type `{foo : Num | a}` or the
  contract `{foo : Num | #a}`.

  We can use `;` to denote the record tail instead, as in `{foo : Num; a}`. `|`
  is working well as a separator for metavalues. Another solution is to use a
  `..a` syntax, that would also convey the idea of a tail. It may clash with
  potential uses for record extension as in say Rust: `Struct {bar: 1, ..rest}`,
  but we would probably use merging in Nickel for that. It also has a different
  meaning in destructuring (this not a parsing problem, but may be confusing for
  the user). `;` is not taken elsewhere, similar to `,` but distinguishable, so
  **we propose to use `;` to denote record tail**.

- **Where to allow a tail**: the only part that is in `Type` but not in `Term`
  is precisely the record tail. Should we allow tails in records that aren't
  the embedding of a type, such as `{foo = fun x => x; a}`? More in the spirit
  of actually mixing terms and types, we could indeed allow it, having records
  storing their tail in a specific attribute, and adapt contract application
  to take it into account. But this implies giving semantics to things like
  `record.is_empty { ; a}` or `{ ; a} & { ; b}`. Those are actually
  interesting questions outside of the context of this RFC (see
  [201](https://github.com/tweag/nickel/issues/201)).

  While allowing tails in arbitrary record may look more uniform, it requires
  more design thinking. There isn't any obvious practical usage that seems to
  require it today. In practice we expect a record with a tail to actually be a
  record type and be used as a type annotation or applied as a contract derived
  from a type, but nothing else. Finally, we can always allow it later on, as it
  should (or, rather, must) be backward-compatible with the simple solution.

  In consequence, we choose to only allow tail on a record that is actually a
  type in the current syntax. In fact we can allow for slightly more room here
  later, so we abstract this notion in the predicate `is_record_type : UniTerm
  -> Bool` and in the end, **propose to set the syntax to require `r = {decl1,
  ..., decln; tail} => is_record_type({decl1, ..., decln})`**. In this RFC,
  we'll define `is_record_type(t)` in the obvious way such as if `t` is a record
  type in the current syntax, i.e. `is_record_type(t) <=> exists. T : Type, T
  record type, t = !T`. In particular `{foo = fun x => x; a}` is now rejected.

The `{_: Num}` syntax is not ambiguous and can be special cased, as it is
already in the current `Type` syntax (it only requires a one character
look-ahead to distinguish).

#### Enums

With the first release in mind, we propose to just disable support for enum
altogether. This is a handy but hardly fundamental feature, and this lets us
more time to find a good syntax replacement. We can also disable `switch`
temporarily, as currently its only usage is for enums.

### Translation

Let's now define the `term` and `type` translations. Our design principle is to
try to have them just be simple syntactic translations that insert the implicit
`#` and `§` appropriately, without having to recurse into terms or do anything
complex, whenever possible.

The two functions must satisfy the following coherence laws with `! : Term +
Type -> UniTerm` the embedding of the current syntax to the new unified syntax
(erasing `#` and `§`, and changing `|` for tails to `;`) and `~` some flavour of
operational equivalence (we won't prove those rules formally, as this a mere
design guideline, so we also restrain from giving a formal definition for `~`).

1. For all `t: Term`, `term(!t) ~ t` and for all `T: Type`, `type(!T) = T` .
   That is, a term or a type in the current syntax with `#` removed will be
   interpreted the same in the new syntax.
2. For all `T: Type`, `term(!T) ~ §T`. That is, the interpretation of something
   that is a type in the current syntax as a term in the new syntax must behave
   the same as the corresponding contract.

### `term`

The `term` translation mainly inserts implicit `§` automatically.

- For most term-only constructs (that is everything excepted
  records and constructors coming from types), `term` is an homomorphism (e.g.
  for lists `term [t1, ..., tn] = [ term(t1), ..., term(tn)]`).
- For type-only constructs (ground types, `->`, and `{_: T}`), it is
  `term(T) = §(type(T))`, that is the contract associated to the type.

#### Records

It remains to treat the case of records, that can mix previously type and term
constructs.

- a row declaration `foo : Num` is translated as it is. This is actually already
  supported since the piecewise signature feature. `{foo : Num}` is just
  equivalent to `{foo | Num}` operationally when `foo` doesn't have a
  definition.
- For a tail, because of the predicate enforced in the grammar, we know that a
  record `{ body; tail}` in `UniTerm` must satisfy `is_record_type({body})`. We
  just convert it to a type, and derive the corresponding contract: `term({body;
  tail}) = §type({body; tail})`.

### `type`

Dually to terms:

- For type-only constructs, `type` is an homomorphism. For example,
  `type(t -> t') = type(t) -> type(t')`.
- For term-only constructs, we take the opaque type associated to a contract.
  That is, `type(t) = #term(t)`. For example,
  `type(fun x => body) = #term(fun x => body)`.

#### Records

As for terms, the case of records remains. For records that are actually the
embedding of a `Type`, such as `{foo : Num, bar: {baz: Num}}`, the translation
is obvious. For records that are the embedding of a `Term` without annotations,
we can simply proceed as above: `type({foo = 1, bar = "baz"}) = #{foo = 1, bar =
"baz"}`.

The case for records that mixes the both `Term` and `Type` is less clear. Take
for example: `x : {foo : Num = 1}`. Should we consider this as a totally opaque
record, that is `type({foo : Num = 1}) = #{foo : Num = 1}` or take advantage
from the information that `foo` is necessarily a number and set `type({foo : Num
= 1}) = {foo: Num}`? The problem with the latter approach is that we loose the
property of *blame safety*, which is that "well-typed program can't be blamed".
Indeed, the extracted type is less precise that the actual contract. For
example, `{foo = 2} : {foo : Num, foo = 1}` would pass typechecking but fails at
runtime on the failure of merging `1` and `2`. There are also multiple
approximation possible: we could create an opaque type encoding the tuple `(Num,
Eq 2)`, we could set `foo : Dyn`, etc.

Because of blame safety, and the different possible trade-offs, **we propose to only translate as
types records that are already record types in the current syntax**. That is:

```
type({f1: t1, .., fn : tn}) = {f1: type(t1), .., fn: type(tn)}
type({...}) = #term({...}) otherwise
```

See [Record types](#record-types) in the [Extensions](#extensions) section for
possible improvements.

#### Ground types and type application

There is currently no general type application per se, but a special casing for
`List Foo`, which is a parametrized type. **We propose to keep ground types as
reserved keywords and special lexemes of the language** for now: it makes things
simpler as we don't have to ask ourselves what `let List = 1 in [] : List Num` should be
interpreted as. That way, we can also special case the application of the list
type and translate an application as a term otherwise:

```
type(a b) = List type(f)   if a == List
          = #term(a b)     otherwise (= #term(a) term(b))
```

This is a straightforward solution but this limits the interaction with type
variables: we can't write things like
`try_to_list: forall a. List (Nullable a) -> Nullable (List a)`, because the
type variable `a` won't "cross" the term application `Nullable _`. Note that,
however, this is already impossible to express in the current syntax, no matter
where you add `#` and `§`. It can be improved on later on in a backward
compatible manner with a proper type application (or different scoping rules for
type variables). What's more, it's not obvious that mixing polymorphic types and
contracts like that would be a widespread practice, as working statically with
contracts is very restricting.

#### Variables

An effect of merging types and terms is that they now share the same variable
namespace. In particular, we can use both type variables or normal variables at
the same place:

```nickel
let f = contracts.from_predicate (fun x => true) in
{fail : forall a. f -> a}
```

Because a type variable never escapes its type, it's impossible to shadow a
`forall a` by a `let`. The converse is possible, as in the variation:

```nickel
let a = contracts.from_predicate (fun x => true) in
{fail : forall a. a -> a}
```

But is not very worrisome: it's clear that the local definition is the one that
matters.

To translate a variable, we thus have to know if a variable is a type variable
or not. We sidestep this question as an external implementation detail, assuming
here that there is a predicate `is_type_var` available during the translation.
We can no set:

```
type(var x) = x   if is_type_var(x)
type(var x) = #x  otherwise
```

## Extensions

We have made a number of proposition guided by simplicity and the idea of
implementing this RFC before the first release, all of this while staying
forward-compatible. This syntax merging proposal brings interesting questions
and perspectives that we sketch in this section.

### Record types

In this RFC, we only translate to record types objects that are already record
types in the current syntax. It turns out we could extract record types from
some record contracts too, that often hold the same information. The typical
example is contracts of the form:

```
{
  foo | #Contract1,
  bar | #{baz | Str},
}
```

Which is perfectly represented by the static type `{foo : #Contract1, bar: {baz:
Str}}`, without breaking blame safety (something with this type will pass the
contract). The general idea is that **a record without any concrete field
definition (`= content`) can be represented accurately by a static type**. The
`type` function on such record would then amount to convert `|` (contract
application) to `:` and drop all others metavalue, and recursively translate the
RHS of annotations to a type.

We could have done this translation in the current proposal already, but one
effect would be to translate the current
`foo | #{bar | #SomeContract, baz | #SomeOther}` to
`foo | {bar: #SomeContract, baz: #SomeOther`. Rigorously, this violates our
first coherence law. Additionally, it turns out the current implementation of
contracts makes the two version above behave slightly differently at runtime.
That this should or should not be the case, we sidestep this difficulty for now
and preserve the previous behavior of record contracts for now.

### Generalized tail

We discussed the question of allowing tails in general records
`{foo | Str = "foo", bar = 1; a}`. I currently don't foresee a use-case for such
objects, but it's interesting to note that adding a specific attribute for the
tail is already what is morally done in the implementation of the contract for a
record with a polymorphic tail such as `forall a. {foo: Num | a} -> {| a}`,
where the polymorphic tail is sealed inside a hidden magic field. Having this
tail represented explicitly could help having a unique implementation for both
record contracts and contracts derived from record types, working both for
as-of-now `Term` (e.g. `{foo | Num, bar | Num}`) and `Type` (e.g.
`{foo : Num, bar : Num | a}`).

### Treatment of variables

Currently we treat all variables `x` in a type position as a contract `#x`
(excepted type variables). However, for static type checking, it could make
sense to do partial substitution, i.e. take as type the actual definition of
`x`.

This would allow to implement type aliases:

```nickel
let PairNum = {fst: Num, snd: Num} in
{fst : PairNum -> Num = fun x => x.fst}
```

As well as extracting more type information from contracts, combined with the
idea from the [Record types](#record-types) section:

```nickel
let Contract = {foo | OtherContr, bar | Str} in
let my_value | Contract = {...} in
// Would typecheck, as my_value has type Contract, which would be expanded to
// {foo | OtherContr, bar | Str} and statically extracted as {foo: OtherContr, bar:
// Str}
let appendToBar : Str -> Str = fun s => my_value.bar ++ s
```

### Type application

We could also have type application as part of the underlying type syntax. The
benefit in allowing it would be to capture type variables, as in
`singleton_maybe: forall a b. Nullable a -> Nullable (List a)`.
We can even imagine the typechecker being able to evaluate function application,
to be able to do something like:

```
let Pair = fun a b => {fst: a, snd: b} in
fst : forall a b. Pair a b -> a = ...
```

With all those objects having consistent interpretation either as term, types,
or contracts.
