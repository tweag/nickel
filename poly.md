# Polymorphic instantiation

```nickel
let ev : (forall a. a -> a) -> Number -> Number = fun f x => f x in
let id : forall a. a -> a = fun x => x in
(ev id) : _
```

- infer the type ev, ..
- check `id` against `forall a. a -> a`
- check `id` against `a0 -> a0`
- check that `a1 -> a1` is a subtype of `a0 -> a0`

`id 5 + 1`
`id : Num -> Num`

- check `id` against `Num -> Num`
- `type_of_id <: checked_type`

```nickel
(let id = fun x = > x in
# id: ?a0 -> ?a0
let ev : (forall a. a -> a) -> Number -> Number = fun f x => f x in
let _ign = ev id in
# forces id : Num -> Num
let _ign = id 5 in
) : _
```


