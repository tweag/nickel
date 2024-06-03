# test.type = 'pass'
let Assert = std.test.Assert in

[
  let AlwaysTrue = fun l t =>
    let boolT | Bool = t in
    if boolT then boolT else %blame% l in
  let AlwaysFalse = fun l t =>
    let boolT | Bool = t in
    if boolT then %blame% l else boolT
  in let not | AlwaysTrue -> AlwaysFalse = fun b => if b then false else true in
  not true == false,

  # id_polymorphic_contract
  let id | forall a. a -> a = fun x => x in id true,

  # higher_order_contract
  let to_bool | forall a. (a -> Bool) -> a -> Bool =
    fun f => fun x => f x in
    to_bool (fun x => true) 4,

  # apply_twice
  let twice | forall a. (a -> a) -> a -> a =
    fun f => fun x => f (f x) in
  twice (fun x => x + 1) 3 == 5,

  # strings
  ("hello" | String) == "hello",
  ("hello" ++ " world" | String) == "hello world",

  # enums_simple
  ('foo | [| 'foo, 'bar |]) == 'foo,
  ('bar | forall r. [| 'foo, 'bar ; r|]) == 'bar,

  # enums_escaped
  ('"foo:baz" | [| '"foo:baz", '"bar:baz" |]) == '"foo:baz",
  ('"bar:baz" | forall r. [| '"foo:baz", '"bar:baz" ; r |]) == '"bar:baz",

  # enums_complex
  let f | forall r. [| 'foo, 'bar ; r |] -> Number =
    match { 'foo => 1, 'bar => 2, _ => 3, } in
  f 'bar == 2,

  let f | forall r. [| 'foo, 'bar ; r |] -> Number =
    match { 'foo => 1, 'bar => 2, _ => 3, } in
  f 'boo == 3,

  let f | forall r. [| 'foo, '"bar:baz" ; r |] -> Number =
    fun x => match { 'foo => 1, '"bar:baz" => 2, _ => 3, } x in
  f '"bar:baz" == 2,

  let f | forall r. [| 'foo, '"bar:baz" ; r |] -> Number =
    fun x => match { 'foo => 1, '"bar:baz" => 2, _ => 3, } x in
  f '"boo,grr" == 3,

  # enums_applied
  # Regression test for enum types converted to contracts outside of annotations
  # causing issues wrt typechecking
  let Wrapper = std.contract.apply [| 'Foo, 'Bar |] in
  ('Foo | Wrapper) == 'Foo,

  # records_simple
  ({} | {}) == {},
  let x | {a: Number, s: String} = {a = 1, s = "a"} in
    %deep_seq% x x == {a = 1, s = "a"},

  let x | {a: Number, s: {foo: Bool}} = {a = 1, s = { foo = true}} in
    %deep_seq% x x == {a = 1, s = { foo = true}},

  # polymorphism
  (let id | forall a. { ; a} -> { ; a} = fun x => x in
    # Note that we have to use `%record/insert%` and `%record/remove%` here, as
    # `record.insert` and `record.remove` enforce the `$dyn_record` contract on
    # their record arguments. `$dyn_record` uses `%record/map%` internally, and
    # mapping over a sealed record is currently forbidden.
    let extend | forall a. { ; a} -> {foo: Number ; a} = fun x => %record/insert% "foo" x 1 in
    let remove | forall a. {foo: Number ; a} -> { ; a} = fun x => %record/remove% "foo" x in

    (id {} == {} | Assert) &&
    (id {a = 1, b = false} == {a = 1, b = false} | Assert) &&
    (extend {} == {foo = 1} | Assert) &&
    (extend {bar = false} == {foo = 1, bar = false} | Assert) &&
    (remove {foo = 1} == {} | Assert) &&
    (remove {foo = 1, bar = 1} == {bar = 1} | Assert) &&
    (remove (extend {}) == {} | Assert) &&
    (extend (remove {foo = 2}) == {foo =1} | Assert) &&
    (let f | forall a b. {f: a -> a, arg: a ; b} -> a =
        fun r => r.f (r.arg) in
      f { f = fun x => x ++ " suffix", arg = "foo" }
      == "foo suffix"
      | Assert)
  ),

  # records_dynamic_tail
  ({a = 1, b = "b"} | {a: Number, b: String ; Dyn}) == {a = 1, b = "b"},
  ({a = 1, b = "b", c = false} | {a: Number, b: String ; Dyn})
  == {a = 1, b = "b", c = false},
  ((fun r => r.b) | {a: Number ; Dyn} -> Dyn) {a = 1, b = 2} == 2,

  # records_open_contracts
  ({a = 0, b = 0} | {a | Number, ..}) == {a = 0, b = 0},
  let Contract = {a | Number} & {..} in
  ({a = 0, b = 0} | Contract) == {a = 0, b = 0},
  let Contract = {..} & {b | Number} in
  ({a = 0, b = 0} | Contract) == {a = 0, b = 0},
  let Contract = {a | Number, ..} & {b | Number, ..} in
  ({a = 0, b = 0, c = 0} | Contract) == {a = 0, b = 0, c = 0},

  # arrays
  ([1, "2", false] | Array Dyn) == [1, "2", false],
  ([1, 2, 3] | Array Number) == [1, 2, 3],
  (["1", "2", "false"] | Array String) == ["1", "2", "false"],

  # full_annotations
  # Check that the contract introduced by the type annotation doesn't interact
  # with the `default` attribute
  ({foo : {bar: Bool} | default = {bar = false}} & {foo.bar = true}).foo.bar,

  # nested_metavalues
  # Regression test for #402
  let MyContract = { x | String } in
  {
    foo | MyContract | default = { x = "From foo" },
    bar | {..} | default = foo
  } == { foo.x = "From foo", bar.x = "From foo"},

  # mixing type and record contracts
  let f | {foo | Number} -> {bar | Number} = fun r =>
    {bar = r.foo} in
  (f {foo = 1}).bar == 1,

  # user-written contract application
  let Extend = fun base label value =>
    let derived = if std.is_record base then
      (base & {foo | Number})
    else
      base in
    std.contract.apply derived label value in
  let Contract = Extend {bar | Number, ..} in
  let Id = Extend (fun label value => value) in
  ({bar = 1, foo = 1} | Contract)
    & ({baz = 1} | Id)
   == {foo = 1, bar = 1, baz = 1},

   # regression tests for #758
  let A = fun param label value => value in
  let B = fun label value => value in
  ([1] | A (Array B)) == [1],
  let B = fun label value => value in
  let Contract = Array B in
  true,

  # Polymorphism and dictionary interaction

  (let nonsense
    | forall free. {_: free -> (forall bound. bound -> bound) } -> free -> {_: (forall bound. bound -> bound) }
    = fun r x => std.record.map_values (fun f => f x) r
  in nonsense { foo = fun _x y => y, bar = fun _x y => y } 5 |> std.record.map_values (fun f => f 7))
  == { foo = 7, bar = 7 },

  # forall constraints
  # this tests that shadowing forall variables is treated correctly

  let g | forall r. { ; r } -> { z : Number ; r }
        = fun r => %record/insert% "z" r 1 in
  let f | forall r. { ; r } -> { g : forall r. { ; r } -> { z : Number ; r }; r }
        = fun r => %record/insert% "g" r g in
  let res = f { z = 3 }
  in std.seq res true,

  # std.contract.Sequence
  let SmallerThanFive = std.contract.from_predicate (fun x => x < 5) in
  let three | std.contract.Sequence [ Number, SmallerThanFive] = 3 in
  three == 3,

  # std.contract.Sequence preserves order
  let tag | std.contract.Sequence [std.enum.TagOrString, [| 'some_tag |]]
    = "some_tag"
  in
  tag == 'some_tag,
]
|> std.test.assert_all
