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

This is verbose and difficult to parse. The issue isn't aesthetic only: when
peer-programming Nickel with beginners, they happened to be confused as why
sometimes we need a hash and sometimes not, why we used `List` and sometimes
`list` (the equivalent of `§List` in this document).

The type application syntax (`List Num`) also coincides with the standard
application within contracts. That is, `§(List Num) ~ §List §Num`. It would thus
make sense to define type application for user-defined contracts by `#Foo Type ~
#(Foo §Type)`, and to be able to write directly `#Nullable Num` or `#Nullable
#Port` instead of `#(Nullable §Num)` and `#(Nullable Port)` respectively.

This RFC proposes to make both terms and types to share the same syntax, such
that we could just write:

```nickel
{
  list_ports_maybe | Nullabe (List Port)
}
```

Which looks more natural, and can be given a reasonable semantics.

## Challenges

Such a change brings in two main challenges:

### Semantics

The first challenge is to give a semantics for all the strange term we can now
write:

```nickel
let foo : forall a. a -> (fun x => x + 1) -> a + 1
```

In order to tame 1., we'll try to rely on the fact that the current language
already has generic transformations `# : Terms -> Types` and `§ : Types ->
Terms`. Our goal is to define a new syntax `UniTerms`, and two translations
`type : UniTerms -> Types` and `term : UniTerms -> Terms`, based on the former
transformations. We will then be able to write Nickel programs in `UniTerms` and
translate them to the previous syntax.

### Conflicts

Syntaxes clashes. Because the syntaxes were separate, we could use the same
constructs for different objects in terms and in types. Most precisely, here is
a list of the current conflicts:

- The pipe operator `|` is used for metavalue and contract annotation in terms,
  and for polymorphic record tails in types. For example, `{foo : Num | a}`
  could now be interpreted as a record with a field `foo` with a `Num`
  annotation, and a polymorphic tail `a`, or it could be interpreted as a
  field `foo` with two annotations, a type annotation `Num`, and a contract
  annotation `a` (that is, `{foo : Num | #a}` is the current syntax).
- The syntax for enum types `<foo, bar>` conflicts with the syntax of comparison
  operator. Typically, `a <foo> bar` can be parsed as `a §(<foo>) bar` or
  `(a < foo) > bar`.
- The syntax for record literals may conflict, as it is used both for record
  types and records themselves. Now, how should we understand `{foo : Bar, bar =
  2}` as a type? And as a term?

## Proposal

### The `UniTerms` syntax

Let us define the `UniTerms` syntax as the union of the two currently separate
syntaxes, but with `#` removed (remember thath `§` isn't currently actually
available, otherwise, we would have removed it too).

#### Records

We need to allow mixing row types syntax and normal types syntax, that is `{foo
: Bar, baz = 1, ..}`, or the parser will have a hard time disambiguating e.g.
`{foo : Num, bar : Num, foo = 1, bar = 1}` (a term with piecewise signatures)
from the previously record type `{foo: Num, bar: Num}`. This requires to resolve
the conflict about `|`: we propose to use `;` to denote the record tail instead:
`{foo : Num; a}`. We could also use a `..a` syntax: it may clash with potential
uses for record extension as in say Rust: `Struct {bar: 1, ..rest}`, but we
would probably use merging in Nickel for that.

The `{_: Num}` syntax may look ambiguous, but this one can be special cased, as
it is the case within the current type syntax (it only requires a one character
look-ahead to decide).

#### Enums

With the first release in mind, which is the scope of this proposal, we propose
to just disable support for enum altogether. This is a handy but hardly
fundamental feature, and this lets us more time to find a good syntax
replacement.

### Translation

Let's now define the `term` and `type` translations. Our design principle is to
try as hard as possible to have them just be simple syntactic translations that
insert the implicit `#` and `§` appropriately, if possible without having to
recurse into terms or do something complex, although that may not be always
possible.

The two functions must satisfies the following coherence laws with `!` the
embedding of the current syntax to the unified syntax (it may have to do some
changes, like chainge `|` to `;`, but can surely easily define an embedding):
`!: Term + Type -> UniTerm` and `~` is some flavour of operational equivalence
(we won't prove those rules formally, as this a mere design guideline, so we
also let the definitions be rough). It basically just erases `#` and `§`.

1. For any `t: Term`, `term(!t) ~ t` and for any `T: Type`, `type(!T) = T` .
   That is, a term or a type in the current syntax with translation operator
   removed will be interpreted the same in the new syntax.
2. For any `T: Type`, `term(!T) ~ §T`. That is, the interpretation as a term of
   something that is a type in the current syntax must be the corresponding
   contract.

### `term`

As explained, we see the `term` translation as inserting implicit `§`
automatically.

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
- For a tail, we have several solution:
    1. Any record with a tail is first translated to a type, and then converted
       to a contract. This follows mechanically the original principle, but
       means the result is not a record: `{foo | Num; a}` would be elaborated to
       a function. That is, `term(r with tail) = §type(r with tail)`.
    2. More in the spirit of actually mixing terms and types, we could have
       records to store their tail in a specific attribute, and adapt contract
       application to take it into account. But this implies giving semantics to
       things like `record.is_empty { | a}` or `{ | a} & { | b}`. Those are
       actually interesting questions outside of the context of this RFC (see
       [201](https://github.com/tweag/nickel/issues/201)).
  Solution 2. sounds more uniform (representing everything as records), but it
  requires more design thinking. There isn't any obvious practical usage that
  seems to require it right now (in practice we expect a record with a tail to
  be only used as a type annotation or applied as a contract, but nothing else).
  Finally, we can always switch to 2. later, as it should be a
  backward-compatible improvement.

  For all those reasons, **we propose option 1. in this RFC**.

### `type`

Dually to terms:

- For type-only constructs, `type` is the obvious homomorphism. For example,
  `type(t -> t') = type(t) -> type(t')`.
- For term-only constructs, we take the opaque type associated to a contract.
  Concretely, we just translate it as a `Term`, and put a `#` in front:
  `type(fun x => body)` = `#term(fun x => body)`.

#### Records

As for terms, the case of records remain. For records that are actually the
embedding of a `Type`, such as `{foo : Num, bar: {baz: Num}}`, the translation
is obvious. For records that are the embedding of a `Term`, we can simply
proceed as above: `type({foo = 1, bar = "baz"}) = #{foo = 1, bar = "baz"}`.

The case for records that mixes the both `Term` and `Type` is less clear. Take
for example: `x : {foo : Num = 1}`. Should we consider this as a totally opaque
record, that is `type({foo : Num = 1}) = #{foo : Num = 1}` or take advantage
from the information that `foo` is necessarily a number and set `type({foo : Num
= 1}) = {foo: Num}`? The problem of the latter is that we loose the property of
*blame safety*, which is that "well-typed program can't be blamed", because the
type deduced is less precise that the actual contract. For example, `{foo = 2} :
{foo : Num, foo = 1}` would pass typechecking but fails at runtime on the
failure of merging `1` and `2`. There are also multiple approximation possible:
we could create an opaque type encoding the tuple `(Num, Eq 2)`, we could set
`foo : Dyn`, etc.

Because of blame safety and the different possible trade-off in translating a
mixed record to a `Type`, **we propose to only translate as types records with
fields that don't have any definition**. That is, beside now allowing `{foo | Num}` to
behave as `{foo : Num}` as a type annotation, and ignoring metadata like
documentation, we only translate as record types expressions that are already a
record type in the current syntax (`Type`). Otherwise, we consider it as an
opaque type, that is `type({ ... }) = #term({ ... })`.

Note that this may create a loop in the translation for mixed record with tails. 
