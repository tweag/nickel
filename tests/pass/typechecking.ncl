let typecheck = [
  // basics
  true : Bool,
  false : Bool,
  0 : Num,
  45 : Num,
  (fun x => x) : forall a. a -> a,
  let x = 3 in (x : Num),
  4 + false,
  (true | Num) : Num,
  "hello" : Str,

  // functions
  (fun x => if x then x + 1 else 34) false,
  let id : Num -> Num = fun x => x in (id 4 : Num),
  // the id parameter is inferred
  (fun id => (id 4 : Num)) (fun x => x),
  // non strict zones don't unify
  (fun id => (id 4) + (id true: Bool)) (fun x => x),
  // contracts are opaque types
  (let AlwaysTrue = fun l t => if t then t else %blame% l in
  ((fun x => x) : AlwaysTrue -> AlwaysTrue)),

  // simple_polymorphism
  let f : forall a. a -> a = fun x => x in
    ((if (f true) then (f 2) else 3) : Num),

  let f : forall a. (forall b. a -> b -> a) = fun x y => x in
    ((if (f true 3) then (f 2 false) else 3) : Num),

  let f : forall a. (forall b. b -> b) -> a -> a = fun f x => f x in
    f (fun z => z : forall y. y -> y),

  // forall nested
  let f : forall a. a -> a =
      let g | forall a. (a -> a) = fun x => x in
      g in
    ((if (f true) then (f 2) else 3) : Num),

  let f : forall a. a -> a =
      let g | forall a. (a -> a) = fun x => x in
      g g in
    ((if (f true) then (f 2) else 3) : Num),


  let f : forall a. a -> a =
      let g : forall a. (forall b. (b -> (a -> a))) = fun y x => x in
      g 0 in
    ((if (f true) then (f 2) else 3) : Num),

  // enums_simple
  (`bla : [|bla |]),
  (`blo : [|bla, blo |]),
  (`bla : forall r. [|bla ; r |]),
  (`bla : forall r. [|bla, blo ; r |]),
  ((switch {`bla => 3} `bla) : Num),
  ((switch {`bla => 3, _ => 2} `blo) : Num),

  // enums_complex
  ((fun x => switch {`bla => 1, `ble => 2} x) : [|bla, ble |] -> Num),
  ((fun x => switch {`bla => 1, `ble => 2, `bli => 4} (%embed% bli x))
    : [|bla, ble |] -> Num),
  ((fun x =>
      (switch {`bla => 3, `bli => 2} x)
      + (switch {`bli => 6, `bla => 20} x))
    `bla
    : Num),

  let f : forall r. [|blo, ble ; r |] -> Num = fun x =>
    switch {`blo => 1, `ble => 2, _ => 3} x in
            (f `bli : Num),

  let f : forall r. (forall p. [|blo, ble ; r |] -> [|bla, bli ; p |]) =
    fun x => switch {`blo => `bla, `ble => `bli, _ => `bla} x in
    f `bli,

  // static records
  ({bla = 1} : {bla : Num}),
  ({blo = true, bla = 1} : {bla : Num, blo : Bool}),
  ({blo = 1}.blo : Num),
  ({bla = true, blo = 1}.blo : Num),
  let r : {bla : Bool, blo : Num} = {blo = 1, bla = true} in
    ((if r.bla then r.blo else 2) : Num),

  let f : forall a r. {bla : Bool, blo : a, ble : a ; r} -> a =
      fun r => if r.bla then r.blo else r.ble in
    (if (f {bla = true, blo = false, ble = true, blip = 1, }) then
      (f {bla = true, blo = 1, ble = 2, blip = `blip, })
    else
      (f {bla = true, blo = 3, ble = 4, bloppo = `bloppop, }) : Num),

  ({ "%{if true then "foo" else "bar"}" = 2, } : {_ : Num}),
  ({ "%{if true then "foo" else "bar"}" = 2, }."%{"bl" ++ "a"}" : Num),
  ({ foo = 3, bar = 4, } : {_ : Num}),

  // seq
  (%seq% false 1 : Num),
  ((fun x y => %seq% x y) : forall a. (forall b. a -> b -> b)),
  let xDyn = if false then true else false in
    let yDyn = 1 + 1 in (%seq% xDyn yDyn : Dyn),

  // lists_simple
  [1, "2", false],
  //TODO: the type system may accept the following test at some point.
  //([1, "2", false] : List),
  ["a", "b", "c"] : List Str,
  [1, 2, 3] : List Num,
  (fun x => [x]) : forall a. a -> List a,

  // lists_ops
  (fun l => %tail% l) : forall a. List a -> List a,
  (fun l => %head% l) : forall a. List a -> a,
  (fun f l => %map% l f) : forall a b. (a -> b) -> List a -> List b,
  (fun l1 => fun l2 => l1 @ l2) : forall a. List a -> List a -> List a,
  (fun i l => %elem_at% l i) : forall a. Num -> List a -> a,

  // recursive_records
  {a : Num = 1, b = a + 1} : {a : Num, b : Num},
  {a : Num = 1 + a} : {a : Num},
  {a : Num = 1 + a} : {a : Num},

  // let_inference
  (let x = 1 + 2 in let f = fun x => x + 1 in f x) : Num,
  // (let x = 1 + 2 in let f = fun x => x ++ "a" in f x) : Num,
  {a = 1, b = 1 + a} : {a : Num, b : Num},
  {f = fun x => if x == 0 then 1 else 1 + (f (x + (-1))),}
    : {f : Num -> Num},

  { f = fun x => if x == 0 then 1 else 1 + (f (x + (-1))),}
    : {f : Num -> Num},

  // polymorphic_row_constraints
  let extend | forall c. { ; c} -> {a: Str ; c} = 0 in
    let remove | forall c. {a: Str ; c} -> { ; c} = 0 in
    (let good = remove (extend {}) in 0) : Num,

  let r | {a: Num ; Dyn} = {a = 1, b = 2} in (r.a : Num),
  ({a = 1, b = 2} | {a: Num ; Dyn}) : {a: Num ; Dyn},

  //Regression test following [#270](https://github.com/tweag/nickel/issues/270). Check that
  //unifying a variable with itself doesn't introduce a loop. The failure of this test results
  //in a stack overflow.
  {gen_ = fun acc x =>
    if x == 0 then
      acc
    else
      gen_ (acc @ [x]) (x - 1)
    }.gen_
    : List Num -> Num -> List Num,

  {f = fun x => f x}.f : forall a. a -> a,

  // shallow_inference
  let x = 1 in (x + 1 : Num),
  let x = "a" in (x ++ "a" : Str),
  let x = "a%{"some str inside"}" in (x ++ "a" : Str),
  let x = false in (x || true : Bool),
  let x = false in let y = x in let z = y in (z : Bool),
  // Regression test following, see [#297](https://github.com/tweag/nickel/pull/297). Check that
  // [apparent_type](../fn.apparent_type.html) doesn't silently convert list literals from `List
  // T` (for `T` a type or a type variable) to `List Dyn`.
  {foo = [1]} : {foo : List Num},
  (let y = [] in y) : forall a. List a,

  // full_annotations
  let x : Num | doc "some" | default = 1 in x + 1 : Num,

  // Typed import
  import "typed-import.ncl" : Num,

  // Regression test for #430 (https://github.com/tweag/nickel/issues/430)
  let x = import "typed-import.ncl"
  in x : Num,

  // recursive_records_quoted
  {"foo" = 1} : {foo : Num},

  // stdlib typechecking:
  strings.length : Str -> Num,
  (strings.length "Ok") : Num,
  (strings.length "Ok" == 2) : Bool,
  // partial application
  (strings.split ".") : Str -> List Str,
  (lists.length [] == 0) : Bool,
  (lists.map (fun x => x ++ "1") ["a", "b", "c"]) : List Str,
] in

true
