# test.type = 'pass'
let typecheck = [
  # basics
  true : Bool,
  false : Bool,
  0 : Number,
  45 : Number,
  (fun x => x) : forall a. a -> a,
  let x = 3 in (x : Number),
  4 + false,
  (true | Number) : Number,
  "hello" : String,

  # functions
  (fun x => if x then x + 1 else 34) false,
  let id : Number -> Number = fun x => x in (id 4 : Number),

  # contracts are opaque types
  # TODO: restore the following tests once type equality for contracts is
  # properly implemented (see https://github.com/tweag/nickel/issues/701 and
  # https://github.com/tweag/nickel/issues/724)

  # (let AlwaysTrue = fun l t => if t then t else %blame% l in
  #  ((fun x => x) : AlwaysTrue -> AlwaysTrue)),

  # simple_polymorphism
  let f : forall a. a -> a = fun x => x in
    ((if (f true) then (f 2) else 3) : Number),

  let f : forall a. (forall b. a -> b -> a) = fun x y => x in
    ((if (f true 3) then (f 2 false) else 3) : Number),

  let f : forall a. (forall b. b -> b) -> a -> a = fun f x => f x in
    f ((fun z => z) : forall y. y -> y),

  # forall nested
  let f : forall a. a -> a =
      let g | forall a. (a -> a) = fun x => x in
      g in
    ((if (f true) then (f 2) else 3) : Number),

  let f : forall a. a -> a =
      let g | forall a. (a -> a) = fun x => x in
      g g in
    ((if (f true) then (f 2) else 3) : Number),


  let f : forall a. a -> a =
      let g : forall a. (forall b. (b -> (a -> a))) = fun y x => x in
      g 0 in
    ((if (f true) then (f 2) else 3) : Number),

  # enums_simple
  ('bla : [|'bla |]),
  ('blo : [|'bla, 'blo |]),
  ('bla : forall r. [|'bla ; r |]),
  ('bla : forall r. [|'bla, 'blo ; r |]),
  (('bla |> match {'bla => 3}) : Number),
  (('blo |> match {'bla => 3, _ => 2}) : Number),

  # enums_complex
  ((fun x => x |> match {'bla => 1, 'ble => 2}) : [|'bla, 'ble |] -> Number),
  ((fun x => %enum/embed% bli x |> match {'bla => 1, 'ble => 2, 'bli => 4})
    : [|'bla, 'ble |] -> Number),
  ((fun x =>
      (x |> match {'bla => 3, 'bli => 2})
      + (x |> match {'bli => 6, 'bla => 20}))
    'bla
    : Number),

  let f : forall r. [|'blo, 'ble ; r |] -> Number =
    match {'blo => 1, 'ble => 2, _ => 3} in
    (f 'bli : Number),

  let f : forall r. (forall p. [|'blo, 'ble ; r |] -> [|'bla, 'bli ; p |]) =
    match {'blo => 'bla, 'ble => 'bli, _ => 'bla} in
    f 'bli,

  # recursive let bindings
  let rec f : forall a. a -> Number -> a = fun x n =>
    if n == 0 then x else if f "0" n == "1" then f x (n - 1) else f x (f 1 n) in
    (f "0" 2 : String),
  let rec f : Number -> Number = fun x => if x == 0 then x else f (x - 1) in
    (f 10 : Number),
  let rec repeat : forall a. Number -> a -> Array a = fun n x =>
    if n <= 0 then [] else repeat (n - 1) x @ [x] in (repeat 3 "foo" : Array String),

  # static records
  ({bla = 1} : {bla : Number}),
  ({blo = true, bla = 1} : {bla : Number, blo : Bool}),
  ({blo = 1}.blo : Number),
  ({bla = true, blo = 1}.blo : Number),
  let r : {bla : Bool, blo : Number} = {blo = 1, bla = true} in
    ((if r.bla then r.blo else 2) : Number),
  # Regression tests for https://github.com/tweag/nickel/issues/888
  {"fo京o" = "bar"} : {"fo京o" : String},
  {foo = 1} : { "foo" : Number},

  let f : forall a r. {bla : Bool, blo : a, ble : a ; r} -> a =
      fun r => if r.bla then r.blo else r.ble in
    (if (f {bla = true, blo = false, ble = true, blip = 1, }) then
      (f {bla = true, blo = 1, ble = 2, blip = 'blip, })
    else
      (f {bla = true, blo = 3, ble = 4, bloppo = 'bloppop, }) : Number),

  ({ "%{if true then "foo" else "bar"}" = 2, } : {_ : Number}),
  ({ "%{if true then "foo" else "bar"}" = 2, }."%{"bl" ++ "a"}" : Number),
  ({ foo = 3, bar = 4, } : {_ : Number}),

  # seq
  (%seq% false 1 : Number),
  ((fun x y => %seq% x y) : forall a. (forall b. a -> b -> b)),
  let xDyn = if false then true else false in
    let yDyn = 1 + 1 in (%seq% xDyn yDyn : Dyn),

  # arrays_simple
  [1, "2", false],
  #TODO: the type system may accept the following test at some point.
  #([1, "2", false] : Array Dyn),
  ["a", "b", "c"] : Array String,
  [1, 2, 3] : Array Number,
  (fun x => [x]) : forall a. a -> Array a,

  # arrays_ops
  (fun f l => %array/map% l f) : forall a b. (a -> b) -> Array a -> Array b,
  (fun l1 => fun l2 => l1 @ l2) : forall a. Array a -> Array a -> Array a,
  (fun i l => %array/at% l i) : forall a. Number -> Array a -> a,

  # recursive_records
  {a : Number = 1, b = a + 1} : {a : Number, b : Number},
  {a : Number = 1 + a} : {a : Number},
  {a : Number = 1 + a} : {a : Number},

  # let_inference
  (let x = 1 + 2 in let f = fun x => x + 1 in f x) : Number,
  # (let x = 1 + 2 in let f = fun x => x ++ "a" in f x) : Number,
  {a = 1, b = 1 + a} : {a : Number, b : Number},
  {f = fun x => if x == 0 then 1 else 1 + (f (x + (-1))),}
    : {f : Number -> Number},

  { f = fun x => if x == 0 then 1 else 1 + (f (x + (-1))),}
    : {f : Number -> Number},

  # polymorphic_row_constraints
  let extend | forall c. { ; c} -> {a: String ; c} = 0 in
    let remove | forall c. {a: String ; c} -> { ; c} = 0 in
    (let good = remove (extend {}) in 0) : Number,

  let r | {a: Number ; Dyn} = {a = 1, b = 2} in (r.a : Number),
  ({a = 1, b = 2} | {a: Number ; Dyn}) : {a: Number ; Dyn},

  #Regression test following [#270](https://github.com/tweag/nickel/issues/270). Check that
  #unifying a variable with itself doesn't introduce a loop. The failure of this test results
  #in a stack overflow.
  {gen_ = fun acc x =>
    if x == 0 then
      acc
    else
      gen_ (acc @ [x]) (x - 1)
    }.gen_
    : Array Number -> Number -> Array Number,

  {f = fun x => f x}.f : forall a. a -> a,

  # shallow_inference
  let x = 1 in (x + 1 : Number),
  let x = "a" in (x ++ "a" : String),
  let x = "a%{"some str inside"}" in (x ++ "a" : String),
  let x = false in (x || true : Bool),
  let x = false in let y = x in let z = y in (z : Bool),
  # Regression test following, see [#297](https://github.com/tweag/nickel/pull/297). Check that
  # [apparent_type](../fn.apparent_type.html) doesn't silently convert array literals from `Array
  # T` (for `T` a type or a type variable) to `Array Dyn`.
  {foo = [1]} : {foo : Array Number},
  (let y = [] in y) : forall a. Array a,

  # full_annotations
  (let x = {val : Number | doc "some" | default = 1}.val in x + 1) : Number,

  # Typed import
  (import "../lib/typed-import.ncl") : Number,

  # Regression test for #430 (https://github.com/tweag/nickel/issues/430)
  let x = import "../lib/typed-import.ncl"
  in x : Number,

  # recursive_records_quoted
  {"foo" = 1} : {foo : Number},

  # stdlib typechecking:
  std.string.length : String -> Number,
  (std.string.length "Ok") : Number,
  (std.string.length "Ok" == 2) : Bool,

  # partial application
  (std.string.split ".") : String -> Array String,
  (std.array.length [] == 0) : Bool,
  (std.array.map (fun x => x ++ "1") ["a", "b", "c"]) : Array String,

  # wildcards
  ("hello" : _) : String,
  ((fun x => x + 1) : _ -> Number) : Number -> Number,
  ({"foo" = 1} : {foo : _}) : {foo: Number},

  # Regression test for #700 (https://github.com/tweag/nickel/issues/700)
  # The (| ExportFormat) cast is only temporary, and can be removed once #671
  # (https://github.com/tweag/nickel/issues/671) is closed
  (std.record.update "foo" 5 {foo = 1}) : {_: Number},

  # contracts_equality
  let lib = {
    Contract = fun label value => value,
  } in
  let Proxy1 = lib.Contract in
  let Proxy2 = Proxy1 in
  [
    {
      identity : lib.Contract = (null | lib.Contract),
      foo : lib.Contract = identity,
      bar : Proxy1 = foo,
      baz : Proxy2 = foo,
      baz' : Proxy2 = bar,

      # composite type
      record: {arrow: Dyn -> lib.Contract, dict: {_: Array Proxy2}} = {
        arrow = fun _x => (_x | Proxy2),
        dict : {_ : _} = {elt = [foo]},
      },

      # local definition
      inline = (let InlineProxy = Proxy2 in foo : InlineProxy),
    },
    # the following tests were initially failing while the ones above weren't,
    # for reasons specific to the handling of type wildcard. We keep both version to
    # make sure that type wildcards behave correctly with respect to contract
    # equality.
    {
      identity : lib.Contract = (null | lib.Contract),
      foo : lib.Contract = identity,
      bar : Proxy1 = foo,
      baz : Proxy2 = foo,
      baz' : Proxy2 = bar,
      inline = (let InlineProxy = Proxy2 in foo : InlineProxy),
    },
  ],
] in

true
