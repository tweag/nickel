# Collection of typing examples

Gather various concrete expressions, corresponding generated constraints and
expected behavior to help steer the design and the trade-offs of the type system
and the type inference algorithm.

## Multiple lower bounds

Situations where one unification variable has multiple lower bounds. In that
case, we compute the max of the lower bounds or fail.

### fst

```nickel
f : forall a. a -> a -> a
x : {foo : {bar: Num}, bar: {baz2: Dyn}}
y : {_: {_: Dyn}}

f x y

# constraints
e: ?a
a: ?a
?a >: {foo : {baz: Num}, bar: {baz2: Dyn}}
?a >: {_ : {_ : Dyn}}

# expected
works
a instantiated to {_ : {_ : Dyn}}
```

## Multiple upper bounds

Situations where one unification variable has multiple upper bounds.

### if with subtyping

Q: what is the rule for `if-then-else` ?

```nickel
fun x =>
  if builtin.is_num x then x + 1 else x

e: ?a -> ?b
x: ?a
?a <: Dyn
?a <: Num
?b >: ?a
?b >: Num

# expected
works
x: Num
```

### incompatible if

```nickel
fun x =>
  if x then x + 1 else 0

e: ?a -> ?b
x: ?a
?a <: Num
?a <: Bool
?b >: Num

# expected
fails
Num <> Bool
```

### record.insert with subtyping

```nickel
fun x =>
  let y : Dyn = null in
  let _ign = record.insert 1 "foo" x in
  record.insert null "bar" x

e: ?x -> ?r
x: ?x
?a1 first instantiation of record.insert
?a2 snd instantiation of record.insert

# fst call gives
Num <: ?a1
?x <: {_: ?a1}

# snd call gives
Dyn <: ?a2 => a2 := Dyn
?x <: {_ : Dyn}
?r := {_ : Dyn}

# resolution, phase 1
# unification-like constraint
?x := {_: ?x1} <: {_: ?a1}
# generates
?x1 <: ?a1
?x1 <: Dyn

# state
Num <: ?a1
?x1 <: ?a1
?x1 <: Dyn

# what do we do? unify? what if ?x1 <: ?a3 ?
# Or we do ?a1 >: max (Num, ?x1), setting ?x1 to Num
# ... ?
# Would it be possible to have ?x1 <: ?a1, ?x2 <: ?a2, ?a1 <: Num, ?a2 <: Dyn
# plus other bounds preventing from doing substitution?

# expected
works
{_: Num} -> {_: Dyn}
```

Questions on this: what constraint do we pick? Do max of lower bound should
always provoke unification, like `max ?a Num`?

### record.insert with subtyping and multiple variables

```nickel
fun x =>
  let var = "foo" ++ "bar" in
  let f = fun u => builtin.is_num u."%{var}" in
  let y : Dyn = null in
  let _ign = record.insert 1 "foo" x in
  let _ign2 = f x in
  let _ign3 = f null in
  record.insert null "bar" x

u: ?u
e: ?x -> ?r
f: ?u -> ?v
x: ?x
?a1 first instantiation of record.insert
?a2 snd instantiation of record.insert

# fst call gives
Num <: ?a1
?x <: {_: ?a1}

# snd call gives
Dyn <: ?a2 => a2 := Dyn
?x <: {_ : Dyn}
?r := {_ : Dyn}

# third call gives
?x <: ?u
?u <: {_: ?u1}
?u1 <: Dyn

#4th call gives
?u >: Dyn

# resolution, phase 1
# unification-like constraint
?x := {_: ?x1} <: {_: ?a1}
# generates
?x1 <: ?a1
?x1 <: Dyn

# state
Num <: ?a1
?x1 <: ?a1
?x1 <: Dyn
?x1 <: ?u1
?u1 <: Dyn
?u1 >: Dyn

# expected
works
{_: Num} -> {_: Dyn}
```

## Both lower and upper bounds

### fst plus different annotations

```nickel
let fst : forall a. a -> a -> a in

let foo = fst bar baz in

(foo : T1) + (foo :  T2)

a: ?a
bar: Tbar
baz: Tbaz

?a >: Tbar
?a >: Tbaz
?a <: T1
?a <: T2

# expected
Depends, if max(Tbar, Tbaz) exists and max(Tbar, Tbaz) <: T1, T2 then ok
else fail
```
