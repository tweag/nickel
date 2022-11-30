let {check, ..} = import "testlib.ncl" in

[
  # accesses
  ({foo = 3, bar = true}).bar == true,
  {"%{if true then "foo" else "bar"}" = false, bar = true}.foo
    == false,

  ({foo = 3, bar = true})."bar" == true,
  {"%{if true then "foo" else "bar"}" = false, bar = true}."%{"foo"}"
   == false,

  (record.insert "foo" true {bar = 3}).foo == true,

  # primitive_ops
  record.has_field "foo" {foo = 1, bar = 2},
  record.has_field "fop" {foo = 1, bar = 2} == false,
  ({foo = 2, bar = 3}
  |> record.remove "foo"
  |> record.has_field "foo")
    == false,

  {bar = 3}
  |> record.insert "foo" 1
  |> record.has_field "foo",

  # lazyness of map
  (record.map (fun x y => y + 1) {foo = 1, bar = "it's lazy"}).foo
    == 2,

  let r = record.map
      (fun y x => if %typeof% x == `Num then x + 1 else 0)
      {foo = 1, bar = "it's lazy"} in
    (r.foo) + (r.bar) == 2,

  # merging
  {a = 1} & {b=true} == {a = 1, b = true},
  {a = 1, b = 2} & {b = 2, c = 3}
    == {a = 1, b = 2, c = 3},

  {a = {b = 1}} & {a = {c = true}}
    == {a = {b = 1, c = true}},

  {a = `A, b = `B} & {a = `A, c = `C}
    == {a = `A, b = `B, c = `C},

  # merge_complex
  let rec1 = {
      a = false,
      b = if true then (1 + 1) else (2 + 0),
      c= ((fun x => x) (fun y => y)) 2,
    } in
    let rec2 = {
      b = ((fun x => x) (fun y => y)) 2,
      c = if true then (1 + 1) else (2 + 0),
      d = true,
    } in
    let result = {
      a = false,
      b = 2,
      c = 2,
      d = true,
    } in
    rec1 & rec2 == result,

  # merge_with_env
  (fun y => ((fun x => {a=y}) 1) & ({b=false})) 2
    == {a = 2, b = false},

  # merge_with_env_nested
  {b={c=10}} & ((fun x => {a=x, b={c=x}}) 10)
    == {a=10, b = {c = 10}},

  # recursive_records
  {a = 1, b = a + 1, c = b + a} == {a = 1, b = 2, c = 3},
  {f = fun x y =>
      if x == 0 then y else f (x + (-1)) (y + 1)
    }.f 5 5
    == 10,

  let with_res = fun res =>
    {
      f = fun x =>
        if x == 0 then
          res
        else g x,
      g = fun y => f (y + (-1))
    }.f 10 in
  with_res "done" == "done",

  # piecewise signatures
  {
      foo : Num,
      bar = 3,
      foo = 5
  }.foo == 5,
  {
      foo : Num,
      foo = 1,
      bar : Num = foo,
  }.bar == 1,
  let {foo : Num} = {foo = 1} in foo == 1,

  # recursive overriding with common fields
  # regression tests for [#579](https://github.com/tweag/nickel/issues/579)
  # and [#583](https://github.com/tweag/nickel/issues/583)
  ({foo.bar = foo.baz, foo.baz | default = 2} & {foo.baz = 1}).foo.bar == 1,
  let Name = fun b label name => if b then "hijack" else contract.blame label in
  let Config = {
      b | Bool,
      name | Name b,
  } in
  let data | Config = {
      b = true,
      name = "name",
  } in
  data.name == "hijack",
]
|> check
