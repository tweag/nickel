# Int

WIP. Notes on trying to get our head around an issue involving lazy record
contracts, recursive record, and polymorphic sealing ().

## Playgroud

```
{ foo = 1, bar = foo + 1} |> record.map (fun _key => (+) 1)
```

- type: forall a b. (String -> a -> b) -> {_: a} -> {_: b}

Evaluation of the type to a contract:

- `forall a`: insert `(a <= $forall_var true 0, ..)`
- `forall b`: insert `(b <= $forall_var true 1, ..)`

First argument type : `(String -> a -> b)` transformed to `String -> unseal 0 ->
seal 1`.

`{_: a} ~> {foo | seal 0, bar | seal 0}`
`{_: b} ~> {foo | unseal 1, bar | unseal 1}`

Record rewritten to (after dictionary contract application, before mapping):

`{ foo | seal 0 = 1, bar | seal 0 = foo + 1}`

(but in fact, we probably need `{ foo | !a = 1, bar | !a = foo + 1}`)

Then evaluated to the fixpoint (normal record):
`RECORD{ foo = 1 |> seal 0, bar = (1 |> seal 0) + 1 |> seal 0}`

Final result:

```nickel
{
  foo =
  (1 |> seal 0)
  |> unseal 0
  |> f "foo"
  |> seal 1
# second contract
  |> unseal 1

  bar
  bar =
  (1 |> seal 0) + 1 |> seal 0
  |> unseal 0
  |> f "bar"
  |> seal 1
# second contract
  |> unseal 1
}
```

Run of a `%record_map% { foo | Num =  1, bar | PosNat | Even = foo + 1} (fun _key ((+) 1)`

```
App(Op1(record_map, record), function)
Op1(record_map, record); {stack..}
record ~ RecRecord {..}

RECORD{ foo |pending Num = %1, bar |pending [PosNat,Even] = %2}
%2 := (%assume% Num label %1) + 1

RECORD{ foo = f (%assume% Num %1), bar = f (%assume% PosNat,Even %2) }

# With a call to pending_contract.dualize()

# • type: forall a b. (String -> a -> b) -> {_: a} -> {_: b}
# forall. a (a <= $forall_var true 0, ..)
# forall. b (b <= $forall_var true 1, ..)
#
# String -> unseal 0 -> seal 1)
# : (String -> a -> b)
#
# : {_: a}
# : ~> {foo | seal 0, bar | seal 0}
#
# : {_: b}
# : ~> {foo | unseal 1, bar | unseal 1}
#

{ foo | (seal 0, a, ..data_abouta) = 1, bar | (seal 0, a, ..data_abouta) = foo + 1}

(a -> Dyn)
RECORD{ foo = 1 |> seal 0, bar = (1 |> seal 0 |> a.dualize()) + 1 |> seal 0}

Contract
|> Contract.dualize()

{
  foo =
  (1 |> seal 0)
  |> unseal 0
  |> f "foo"
  |> seal 1
# second contract
  |> unseal 1

  bar
  bar =
  (1 |> seal 0) + 1 |> seal 0
  |> unseal 0
  |> f "bar"
  |> seal 1
# second contract
  |> unseal 1
}
```

## Composition law for contracts

**Goal**: forall T "type closure", M a term in weak head normal form,
`M | T | T.dualize() = M | T'`.
Put differently, with `.` being contract composition: `T.dualize() . T ~ T'`

**Subgoals**:
  - `T.dualize().dualize() ~ T`
  - `(Foo -> Bar)' ~ Foo.dualize()' -> Bar'`

where:
- `.` is contract composition, defined as `M | (T' . T) ~ M | T | T'`
- A type closure is a type `T` with potential free variables together with a
  environment mapping free variable identifiers to a polarity (positive or
  negative) and an uid integer such that distinct variables have distinct
  identifiers. All free variables of `T` must mapped by the environment.
- T' is T where free type variables occurrences (e.g. `a`) have been replaced
  with `flip_compose(a, forall_pol, sym, current_pol)` (see below for the
  definition of `flip_compose`)
- `T.dualize` is defined as the domain contract in `T -> Dyn`.
- contracts are assumed to be idempotent
- `flip_compose(a, forall_pol, sym, current_pol)`:
  - if `forall_pol` and `current_pol` are equal, then gives
    `%seal% sym (%unseal% sym <.>)`
  - if `forall_pol` and `current_pol` aren't equal, then gives
    `%unseal% sym (%seal% sym <.>) ~ id`

## Proof

We proceed by structural induction of types.

- If `T` is a base type or a flat type, there is nothing to do, because
  `T.dualize() = T` and contracts are idempotent.

- If `T = a`, and if `forall_pol` is positive, then `<.> | T` is
  `%unseal% sym <.>`, where `(forall_pol, sym) = Tenv(a)`. Then `T.dualize()` is
  `%seal% sym <.>`, and `T.dualize() . T = %seal% sym (%unseal% sym <.>)`.
  QED.

  If `forall_pol` is `negative`, then `T.dualize()` is `%unseal% sym <.>`, and
  `U := T.dualize() . T = %unseal% sym (%seal% sym <.>) ~ id`. QED.

- If `T = Dom -> Codom`, then `T.dualize()` is the domain contract of
  `(Dom -> Codom) -> Dyn`. The domain contract of `(Dom -> Codom) -> Dyn` is the
  same as the contract of `Dom -> Codom` with polarity flipped. It's given by
  ```text
    fun x => Codom.dualize() (<.> (Dom.dualize().dualize() x))
  ~ fun x => Codom.dualize() (<.> (Dom x))
  ```
  because `dualize()` is an involution.

  Then

  ```text
  T.dualize() . T ~ fun x => Codom.dualize() ((<.> | T) (Dom x))
  T.dualize() . T ~ fun x => Codom.dualize() ((fun y => Codom (<.> (Dom.dualize() y))) (Dom x))
  ```

  Applying beta reduction:
  `fun x => Codom.dualize() (Codom (<.> (Dom.dualize() (Dom x))))`

  If we suppose that `DomDual.dualize() = Dom` and dualize is an involution,
  then by structural induction `Dom.dualize() . Dom ~ Dom'` and
  `Codom.dualize() . Codom ~ Codom'`, giving:

  ```text
  T.dualize() . T ~ fun x => Codom' (<.> (Dom' x))
  ```

  TODO:
  We need to prove that `T' ~ Dom'.dualize() -> Codom'`. Left for a later goals, add
  to subgoals.
