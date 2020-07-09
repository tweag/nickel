# Simple notes on Nickel's type system and typechecking algorithm

**[WARNING] The typesystem described in these notes is not on par with the current one: the latter for example features foralls, record types with row polymorphism, and will probably continue to evolve. However some aspects of the typechecker are similar and these notes are still informative.**

### Goals

What should the user be able to do with this type system:
 * Statically type critical parts of their code, any error there should fail as early as possible;
 * Statically type code, that uses untyped (or untypable) code;
 * Have certainty that evil users that don't statically type their code won't be able to get too far without an error;
 * Don't get in the middle with other implementations that don't care about types.


### Solution


By providing two special functions, **Promise** and **Assume** we let the user state:
 * Their interest in statically type checking a part of their code;
 * Explicitly telling the type checker, don't worry with this part of the code;
 * Assuring the user that during run time, all of these (maybe undecidable) checks will be complied with.

##### Promise

Whenever we want to use the compiler to typecheck some part of the code, we use `Promise(Type, Term)` this will launch a strict type checking of the `Term` (but not the context where is used, this is checked via contracts), and will fail if the `Term` doesn't have type `Type`.

##### Assume

Whenever we want to tell the compiler, trust me here, we use `Assume(Type, Term)`, which will stop any strict type checking that got here, and just trust the user that `Term` has type `Type`. Additionally, this will be checked via contracts at run time.

##### Type checking algorithm



Our types A, B can be variables (s, t, ...) (_unused_), base types (Num, Bool, Dyn) function types (A -> B) or flat types (#e), a contract used as a type.

Our expressions (e, f, ...) can be variables (x, y, ...), non recursive lets (let x = e in f), lambdas (fun x => e), application (e f), constants (true, false, 1, 2, ...), primitive operations (ite, isZero, isNum, isBool, isFun, blame, +, ...), promises (Promise(A, e)) and assumes (Assume(A, e)).

Let G be a set containing the types given to any variable (G = {x:A, ...}).

Our judgement will be of the form G |s- e: A, where s is the strictness and can be either true (under a Promise) or false (under an Assume). A term that is not surrounded by a Promise is considered to be under an Assume.

```
  x: A in G
------------
 G |s- x: A

G, x: A |s- f: B  G |s- e: C  unify(s, A, C) unify(s, B, D)
-------------------------------------------------------------
              G |s- let x = Assume(A, e) in f: D

G, x: A |s- f: B  G |s- e: C  unify(s, A, C) unify(s, B, D)
--------------------------------------------------------------
              G |s- let x = Promise(A, e) in f: D

 G, x: Dyn |s- f: B  G |s- e: C  unify(s, B, D)
---------------------------------------------------------------
   G |s- let x = e in f: D  e not in {Assume, Promise}

 G, x: A |s- e: B unify(s, A, C) unify(s, B, D)
------------------------------------------------
            G |s- fun x => e: C -> D

 G |s- e: A  G |s- f: B unify(s, A, B -> C)
-----------------------------------------------------------
            G |s- e f: C

-----------------           ----------------
 G |s- t/f: Bool             G |s- n: Num

Operations as expected....

 G |true- e: A unify(s, B, A)
------------------------------
  G |s- Promise(A, e) : B

 G |false- e: C unify(s, B, A)
------------------------------
  G |s- Assume(A, e) : B

TODO maybe we can simplify it with this rule

 G |s- e: A unify(s, A, B)
---------------------------
        G |s- e: B

```

Where `unify` is defined as:

```
unify(false, _, _) = true
unify(true, A, B) = A == B, if neither is a flat type
unify(true, #x, #y) = x == y, where x and y are term variables
unify(true, #e, #f) = false, if either e or f is not a variable
```

As soon as we start seeing the algorithm we realize, depending on the strictness, some of these checks seem useless. But the trick is we're doing two different things at the same time, traversing the term in look for Promises and Assumes, and type checking if we're under a Promise.

Another interesting thing is that is very deterministic, but if we introduce type variables I claim (without any kind of proof) that there is a most general derivation. And the implementation chooses that one.

### Desired Properties

 * If `Promise(ty, t)` passes, then it can't happen that a term, containing `Assume(ty, t)` as a subterm, raises positive blame on that `Assume`. No context property for now.
 * For a term `t` without Assumes, then `G |true- t: A` works as expected (progress, preservation, principal type?? (or something similar modulo Dyn), ...)
