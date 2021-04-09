---
feature: overriding
start-date: 2021-03-18
author: Yann Hamdaoui
---

# Overriding

This document is a proposal for an overriding mechanism in Nickel. It is
expected to evolve while we put these ideas in practice, but shall serve as
a design and implementation baseline.

Related issues: [#103](https://github.com/tweag/nickel/issues/103),
[#240](https://github.com/tweag/nickel/issues/240),
[#255](https://github.com/tweag/nickel/issues/255),
[#279](https://github.com/tweag/nickel/issues/279).

## Context

### Definition of the problem

*Overriding* is the ability to take an existing configuration - in our case, a
record - and update a specific part of it, such as a field or the field of a
nested record.

A naive and unergonomic of way of doing it is to repeat all the original
values in a new record:

```
let record = {
  a = 1,
  b = "str",
  c = false
} in
{
  a = record.a,
  b = record.b,
  c = true
}
```

This quickly become unmanageable for large records, which are common in Nix and
Nixpkgs. In functional languages, this is usually better done using what's
called functional update. Functional update is an operation that takes the
original record, one or several field to update, the corresponding new value,
and return a new updated record. It has the same semantics as our first snippet,
but doesn't require to rewrite all unchanged fields.

It can have a builtin syntax, such as OCaml's `with`: `{record with field =
new_value}`, Haskell's `record {field = newValue}`, Nix `//` operator `record //
{field = newValue}`, or Rust's syntax `RecordDataType {field: new_value,
..record}`. There are more advanced programming techniques that make updating
deeply nested records ergonomic such as
[Lenses](https://www.fpcomplete.com/haskell/tutorial/lens/) in Haskell, but
these rely too heavily on advanced language and typing features to be practical.

In Nickel, we can already do a functional update using merging, although the
updated field must have been marked as `default`:

```nickel
let record = {
  a = 1,
  b = "str",
  c | default = false,
} in
record & {c = true}
```

As explained in the next section though, this is not satisfying.

### Overriding recursive records

Nickel's records are different from the ones of OCaml, Haskell or Rust.  They
are lazy and recursive by default. They are thus better understood as
[codata](https://link.springer.com/chapter/10.1007/978-3-030-17184-1_5)
rather than data. Take the following example:

```nickel
let record = {
  protocol = `Http,
  port | default = switch protocol {
    `Http => 80,
    `Ftp => 21,
    _ => 8181,
  },
} in
record & { protocol = `Ftp }
```

Here, our final record will be ``{protocol = `Ftp, port = 80}``. `port` is an
expression depending on `protocol`, but it is not updated in the new version,
which is something one could intuitively expect from a recursive record. Such
recursive update is a pattern actively used in Nix, and is probably a common
scenario for configurations in general: if I patch a base configuration by
changing an option, I usually want other default values depending on this option
to be updated as well.

## Overriding in Nix

Nix expressions don't have a built-in way of overriding recursive records while
automatically updating the dependent recursive fields either. Since the feature
is still actively needed, it is provided by mechanisms implemented in library
code.

Keeping a language lean is usually a good design guideline: to provide an
expressive yet simple set of features upon which others can be built as
libraries. However, in the case of Nix, overriding is a fundamental pattern, and
having it implemented in user code leads to some well-known issues:

- Definition, update or field access may not be as ergonomic as with vanilla
  native records.
- Several competing mechanisms have been implemented in slightly different
  contexts. Overriding in general is already difficult for beginners, but having
  to learn several ones, all similar but still different, is hard.
- User land implementations don't have a first-class access to information such
  as source location and metavalues. In a dynamic language, this can make good
  error reporting much harder.
- It is also potentially harder to make user land implementations efficient.

See [this gist](https://gist.github.com/edolstra/29ce9d8ea399b703a7023073b0dbc00d)
for more details. We continue with an overview of existing mechanisms in Nix and
related languages.

### Nixpkgs overriding

The basic idea is to represent a recursive record explicitly as a function from
a `self` record to the final record:

```nix
r = rec {
  a = 1;
  b = a + 1;
}
# r is represented as:
rRepr = self: {
  a = 1;
  b = self.a + 1;
}
```

`self` is a self-reference, in the style of `this` in object oriented languages.
It is computed as a
[fixpoint](https://en.wikipedia.org/wiki/Fixed-point_combinator), simply
realized by auto-application thanks to laziness:

```nix
r = let fixpoint = rRepr fixpoint; in fixpoint
```

The explicit dependency to `self` gives the ability to provide a different
definition for `self.a` in the final value. Overriding is achieved exactly by
functionally updating the `a` field of the `self` parameter before passing it to
the original representation:

```nix
let extension = {a = 2;}; in
# The fixpoint of result is { a = 2; b = 3; }
resultRepr = self: (rRepr (self // extension)) // extension
```

The second outer update ensures that the final result is also set to `a = 2`,
and not only the `a` appearing in `b`.

Some details are left out, but this is the gist of it. See also
[this pill on overriding](https://nixos.org/guides/nix-pills/override-design-pattern.html)
or [this article on fixpoints in Nix](http://r6.ca/blog/20140422T142911Z.html).

**limits**

- Use a specific representation, rather than handling good old plain records
  (although the actual representation in Nixpkgs is more ergonomic than a plain
  function).
- Overriding nested attribute sets is painful. If one do the naive thing,
  the whole subrecord is erased:
  ```nix
  let rRepr = self: {
    a = {b = self.a.c;};
  }; in
  let extension = {a = {c = 2;};}; in
  rExt = let fixpoint = rRepr (fixpoint // extension); in
  fixpoint // extension
  # Gives {a = {c = 2;};} instead of expected {a = {b = 2; c = 2;};}
  ```
- They don't [compose well](https://github.com/NixOS/nixpkgs/issues/16394).

### Nixpkgs overlays

[Overlays](https://nixos.wiki/wiki/Overlays) were defined to address the
composability issue of the previous mechanism, that is, being able to apply
several overrides in a modular way. Overlays can be seen as a sequence of
transformations from a base record, each layer having access to a `super`
reference to the previous layer and the `self` reference to the final value.

Take two consecutive transformations:

1. Set the `a` field to `1`
2. Set the `b` field to `1`, add `1` to the `a` field

```nix
let baseRepr = self: {c = self.a + self.b;}; in
let overlay1 = self: super: {a = 1;}; in
let overlay2 = self: super: {b = 1; a = super.a + 1;}; in
let applyOverlays = self:
  let base = baseRepr self; in
  let first = base // overlay1 self base; in
  let second = first // overlay2 self first; in
  second; in
let fixpoint = applyOverlays fixpoint; in fixpoint
```

In practice, the `super // ..` and fixpoints parts can be factorised in
dedicated helper functions.

**limits**
 - Modifications must be applied in the right order.
 - Still need to manipulate representations.
 - Overriding nested fields is still clumsy:
   ```nix
   self: super:
   {
     lib = (super.lib or {}) // {
       firefoxVersion = ...;
     };
   }
   ```

### NixOs module system

The NixOS module system takes a different approach. There, all the basic blocks
-- the modules -- are merged together in an unspecified order. What's deciding
the priority of one option over the other are attributes that are explicitly
stated in the modules themselves. The declaration of options (type, priority,
etc.) and their assignement must be made separately.

Nonetheless, modules need to combine different pieces of data together rather
than just overriding one value with another. This is made difficult by the
disappearance of the explicit `super`, compared to overlays.

The issue is solved by custom merge functions, that can define how to combine
different values of the field of a configuration. By default, when merging two
list values, the module system only knows how to replace one value with the
other, because there's no canonical and commutative way of merging two lists.
However, the user can specify that these lists should in fact be concatenated,
resulting in the possibility of defining a list of paths by pieces:

```nix
# Some module
{
  paths = mkOption {
    type = types.listOf types.path;
  };
}

# Config
[..]
someModule.paths = [foo/bar];

# Other config
[..]
someModule.paths = [/bar/baz];

# Resulting config: [foo/bar /bar/baz]
```

## Overriding elsewhere

### CUE
CUE allows a form of late-binding for recursive attributes:

```
$cat test.cue
fields: {
 a: int
 b: a + 1
} & {
 a: 2
}
$cue eval test.cue
fields: {
    a: 2
    b: 3
}
```

Combined with [default values](https://cuelang.org/docs/tutorials/tour/types/defaults/),
this provides an overriding mechanism:

```
$cat test2.cue
fields: {
 a: int | *1
 b: a + 1
} & {
 a: 2
}
$cue eval test2.cue
fields: {
    a: 2
    b: 3
}
```

The basics of this overriding mechanism are close in spirit to what we want to
achieve in this proposal. This is similar to Nickel's current `default`
behavior, but with the desired late-bound merging. This is however not enough
for Nix use cases: we will need to add other features to the merge system.

### Jsonnet

Jsonnet refers to its overriding mechanism as *inheritance*, implemented by the
`+` operator on objects:

```jsonnet
local obj = {
  name: "Alice",
  greeting: "Hello, " + self.name,
};
[
  obj,
  obj + { name: "Bob" },
  obj + { greeting: super.greeting + "!"},
  obj + { name: "Bob", greeting: super.greeting + "!"},
]
```

This is similar to the Nix operator `//`, but doing recursive overriding in the
expected way out of the box. The extension can access the previous version in
the same way as Nixpkgs overlays, using the `super` keyword.

## Taking a step back

While not exactly the same, all these overriding mechanisms are based on the
same underlying principles:
1. Represent recursive records (explicitly or implicitly) as a function of
   `self` (and `super` in some cases)
2. Compute the combination as a **fixpoint** of several recursive records,
   giving back a standard record.

As hinted by Jsonnet's terminology, the semantics of recursive records
strikingly resemble the semantics of objects and classes in OOP.  Replace
records with objects, fields with methods and overriding with inheritance. This
is not so surprising: there's actually an history of encoding objects in
functional languages as recursive records (see the introduction of
[The Recursive Record Semantics of Objects Revisited](https://hal.inria.fr/inria-00072423)
for a good overview) going back to 1988
\[[Cardelli](http://lucacardelli.name/papers/inheritance.pdf)\].

This is also mentioned in the
[README](https://github.com/MuKnIO/nixpkgs/blob/devel/lib/pop.md#some-historical-context)
of POP (an object system in Nix), where the author observes that overriding
mechanisms in Nix (and Jsonnet for that matter) are a simplified lazy object
system (simplified because objects lack proper state and there is no distinction
between classes and instances). Their logical conclusion is to embrace this fact
and design a proper object system helped by existing literature, rather than
reinventing the wheel. Similarly, Nix [overlays](https://nixos.wiki/wiki/Overlays)
can be seen as a single inheritance mechanism.

Inheritance-based overriding imposes an order on the overrides. A single level
inheritance is usually fine, but a complex hierarchy can become hard to maintain
and to reason about.

The NixOS module system is designed differently. It is based on merging: the
configuration is created by combining a set of unordered records following
specific rules. Of course, there's still a need for ordering information
somewhere, but it is rather expressed as priorities. This system has the
advantage of making merge commutative (in contrast with inheritance or the `//`
operator), as in CUE, and to untie data definition from precedence
specification: one can define a module where each field has a different
priority, if it makes sense to group them logically.  With inheritance, values
are required to be grouped in layers of the same priority, instead of logically.

Summing up the differences between inheritance-based mechanisms and merge-based
mechanism:

**order-dependency**

Inheritance is order dependent: the chain of extensions must be defined with the
precise order they will be applied in mind, and definitions must be grouped in
consequence. On the other hand, merging is commutative, and the precedence
information is encoded as priorities. Thus, overriding by merging can be defined
using stand-alone pieces of data, although the behavior of priorities is not
local.

**super**

Merged records have only access to the final computed fixpoint `self`, while
objects have access to the previous stage of extension via `super`. However, as
in the NixOS module system, it is possible to address this issue using custom
merge functions. This is a bit less powerful, but in a good way: it forces the
merge strategy to be uniform along each field, while overlays can do pretty much
what they want.

## Proposal

We basically propose to adopt the same kind of merge, priority, and custom merge
function-based scheme as the NixOS module system. One big difference with the
NixOS module system is that it would be built-in in the language, being usable
for any native record whatsoever, and having good interaction with the rest of
the language features. The implementation would have first class access to
locations, to the AST, the memory layout, the metadata, and so on.

We first describe ideas at a high-level. Issues, details and challenges are
discussed further in a dedicated section.

### Recursive records & merging

**This section defines the semantics, rather than an actual efficient
implementation**.

As before, it is useful to see recursive records represented as functions -- or
constructors -- that take a `self` parameter and return a final non recursive record,
in the same way as the original overriding mechanism of Nixpkgs (the non-existing
syntax `def := value` is used to insist on the fact that we are defining new objects):

```
r = {
  a = 1;
  b = a + 1;
}
// Definition of the representation of r
repr(r) := fun self => {
  a = 1;
  b = self.a + 1;
}
```

Field access amounts to compute a fix-point:

```
repr(r).foo := let fix = repr(r) fix in fix.foo
```

Merging simply merges the underlying representations:

```
repr(r1) & repr(r2) := fun self => r1 self & r2 self
```

This brings the problem of scoping: should a record be able to access a yet
undefined field because it is expected to be provided by a subsequent merge?
Two possible approaches:

 - **dynamic scoping**: records can reference fields that are not explicitly
   defined locally, such as:
   ```nickel
   {a = b} & {b = 1}
   ```
   However, dynamic scoping have a number of issues, and is usually considered
   bad practice.
 - **lexical scoping**: as currently, require self-referenced fields to be
   defined locally. Note that thanks to contracts, one can require the existence
   of a field without defining it. For example, we could write the previous
   example as:
   ```nickel
   {a = b, b | Num} & {b = 1}
   ```
   This is also a better practice to explicitly state the fields whose presence
   is required.

### Priorities

Merge is fundamentally commutative. The problem is, not all fields should be
treated the same: some are default values that ought to be overridden, some are
high-priority values that ought to override. The solution is to use a priority
system, encoding the precedence of each value while retaining commutativity
(only the priority annotation counts, not the order in which values are merged).

Priorities have drawbacks. One is non-locality: the final result depends on
other priorities of the field's values you are being merged with, potentially
defined elsewhere. However, this can be mitigated by good defaults and an
adapted set of priorities. For example:

1. `default` is lowest than everything else (bottom)
2. Integer priorities in the middle, with `0` being the default
3. `force` is higher than everything else (top)

Doing so, it's easy to just use `default` or `force` to override or be
overridden by anything, without knowing the precise integer priority. Integer
priorities still gives freedom with an infinite supply of levels if that is
required, and the default priority is right in the middle.

However, as noted in [#240](https://github.com/tweag/nickel/issues/240),
configuration should be easily overridable, and the approach outlined here can
become annoying because it may end up requiring either configurations to be
written with `default` or `force` everywhere. A solution is to have additional
recursive (or "leafy", or "push down") annotations, as described in
[#279](https://github.com/tweag/nickel/issues/279):

```nickel
let someNeutraleConfig = {
  foo = 1,
  bar.baz = "stuff",
  bar.blorg = false,
}
let defaulted | default rec = someNeutralConfig;

// defaulted will evaluate to:
// {
//   foo | default = 1,
//   bar = {
//     baz | default = "stuff",
//     bar.blorg | default = false,
//   },
// }
// This is different from defaulted | default !

// Same with force:
let overrider | force rec = someNeutralConfig;
```

This way, an existing definition (arbitrarily complex: that could be the root of
all Nixpkgs) can easily (and lazily) be turned into a full overriding or
overridable configuration.

### Custom merge functions

Custom merge functions would be specified by a `merge` metavalue attribute. In
order to enforce commutativity, they would receive their arguments in an
indistinguishable order, such as `{lower: Dyn, higher: Dyn}`, just knowing which
one has a higher priority than the other. If both have the same priority, the
order is not specified, and may even be randomized by the interpreter.

```nickel
let mergeLists : forall a. {lower: List a, higher: List a} -> List a = fun args =>
  args.lower @ args.higher in

let Contract = {
  path | List Str
       | merge mergeLists
       | doc "A list of paths to search in."
} in

let block1 | #Contract = {
  path = ["/usr/local/bin"]
} in
let block2 = {
  path = ["/bin"]
} in

// { path = ["usr/local/bin", "/bin"] }
block1 & block2
```

## Issues & challenges

The proposal raises some questions. While none seems insuperable, users will
need to have a good mental picture of the system, and we should be careful to
avoid ending up with a complicated system full of ad-hoc fixes to edge cases.

### Implementation

Recomputing the fixpoint at each field access is wasteful, because recursive
records as functions satisfy the following property: as long as it is not
merged, the `self` argument is constant. Thus, we can memoize the fixpoint and
only invalidate `self` on a merge. Also, fields that do not depend on a
recursive variable can be hoisted out of the function, using a representation
like:

```
repr(r) := {
  a = 1;
  b = fun self => self.a + 1;
}
```

Concretely, each field of a record may either be a thunk as usual, in the case
of non recursive expressions like `a`, or a thunk together with the original
expression in the recursive case. All record operations excepted merge operate
on the thunk. A merge operation, on the other hand, restores the original
expression again.

There's more potential optimizations, but this first step should be a reasonable
trade-off between implementation complexity and performance. See
[#103](https://github.com/tweag/nickel/issues/103) for more details.

### Priorities

In some cases, the semantics of the proposed recursive annotations `default rec`
and `force rec` are not obvious:

```nickel
let record = {
  a = 1,
  b | force = "DONT CHANGE",
} in
record | default rec
```

What should be the result? Several possibilities:

1. The `default` erases everything else. This doesn't sound right, because a
   configuration may use `force` to prevent a final value from being modified.
2. The `default` only erases the `0` priority, and let any other untouched.
3. The `default` erases everything excepted a `force`.

Both 2. and 3. sounds reasonable. The `force rec` may have a dual behavior, but
could also erase everything else: it make more sense to turn a `default` to a
`force` than the other way around.

### Custom merge functions

In the case of the NixOS module system, all the configuration is merged in one
final phase that conceptually evaluates the merge AST.

In Nickel, merging can be done partially and observed by expressions. The
natural semantics of custom merge functions should be to affect all subsequent
merging, as for contracts application. Indeed, we want to specify a contract or
a custom merge function once and for all, not to repeat it at every field
definition:

```nickel
let add = fun args => args.lower + args.higher in
// {a = 3}
{a | merge add = 1} & {a = 1} & {a = 1}
```

However, this breaks commutativity, because it affects terms on the right but
not on the left:

```
let r1 = {a = 1} in
let r2 = {a = 1} in
let r3 = {a | merge add = 1} in

// {val = 2}
r1 & r2 & r3

// {val = 3}
r3 & r1 & r2
```

Possible solutions:

1. Make a `n-ary` merging behave differently than repeated merging, which would
   apply the custom merge function to all arguments. This doesn't solve the
   commutativity problem per se: we also have to decide that in ordinary
   merging, custom function wouldn't affect subsequent merges anymore. This does
   raise yet other problems, one being of a good distinctive syntax. More
   generally, it sounds confusing and unsatisfying.

2. What seems the right, but not trivial solution, would be to symmetrize custom
   merging and make it "distribute backward" as well, that is (simplifying by
   forgetting about priority and `lower`/`higher` arguments):

   ```nickel
   val1 & val2 & (val3 | merge func)
   <=> (val1 | merge func) & val2 & val3
   <=> func (func val1 val2) val3
   // instead of the naive
   <=/=> func (val1 & val2) val3
   ```

   But what if `val1 & val2` is in a thunk that happens to be forced in between?
   What should `let x = val1 & val2 in x & (val3 | merge func)` evaluate to? And
   `let x = val1 & val2 in builtins.seq x (x & (val3 | merge func))`?

   In some way, we would like to have a call-by-name semantics rather than a
   call-by-need. In the case of record fields, overriding has to solve the
   same problem of remembering an original expression along an evaluated
   version.  If we allow custom merge annotations only on record fields, it is
   possible to always recover the original merge AST (abstract syntax tree) and
   interpret it with the custom merge function as wanted. This may prove a bit
   tricky to implement in practice, for the merge expression `val1 & val2` may
   be obfuscated by some program transformations or evaluation artifacts, but
   doable.

3. A more extreme take is to do as in 2., but generalize it to every term: a
   merge expression would behave like a lazy datatype `Merge(t1,t2)` with
   respect to evaluation. Evaluating it would amount to automatically apply
   `eval : Merge(t1,t2) -> t1 & t2`, which wouldn't erase the original top-level thunk
   `Merge(t1,t2)`.  This is exactly how default value are currently handled, for
   example. Doing so, we don't need to restrict custom merge functions to record
   fields. This incurs an additional cost though, of remembering all the ASTs of
   merge expressions.

I propose to adopt solution 2., as it is probably sufficient in the vast
majority, and is still compatible with a future upgrade to 3.
