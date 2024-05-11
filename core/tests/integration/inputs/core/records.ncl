# test.type = 'pass'


[
  # accesses
  ({ foo = 3, bar = true }).bar == true,
  { "%{if true then "foo" else "bar"}" = false, bar = true }.foo == false,
  ({ foo = 3, bar = true })."bar" == true,
  { "%{if true then "foo" else "bar"}" = false, bar = true }."%{"foo"}" == false,
  (std.record.insert "foo" true { bar = 3 }).foo == true,

  # primitive_ops
  std.record.has_field "foo" { foo = 1, bar = 2 },
  std.record.has_field "fop" { foo = 1, bar = 2 } == false,
  (
    { foo = 2, bar = 3 }
    |> std.record.remove "foo"
    |> std.record.has_field "foo"
  ) == false,
  { bar = 3 }
  |> std.record.insert "foo" 1
  |> std.record.has_field "foo",

  # laziness of map
  (std.record.map (fun x y => y + 1) { foo = 1, bar = "it's lazy" }).foo == 2,
  let r =
    std.record.map
      (fun y x => if %typeof% x == 'Number then x + 1 else 0)
      { foo = 1, bar = "it's lazy" }
  in
  (r.foo) + (r.bar) == 2,

  # merging
  { a = 1 } & { b = true } == { a = 1, b = true },
  { a = 1, b = 2 }
  & { b = 2, c = 3 } == { a = 1, b = 2, c = 3 },
  { a = { b = 1 } }
  & { a = { c = true } } == { a = { b = 1, c = true } },
  { a = 'A, b = 'B }
  & { a = 'A, c = 'C } == { a = 'A, b = 'B, c = 'C },

  # merge_complex
  let rec1 = {
    a = false,
    b = if true then (1 + 1) else (2 + 0),
    c = ((fun x => x) (fun y => y)) 2,
  }
  in
  let rec2 = {
    b = ((fun x => x) (fun y => y)) 2,
    c = if true then (1 + 1) else (2 + 0),
    d = true,
  }
  in
  let result = {
    a = false,
    b = 2,
    c = 2,
    d = true,
  }
  in
  rec1 & rec2 == result,

  # merge_with_env
  (fun y => ((fun x => { a = y }) 1) & ({ b = false })) 2 == { a = 2, b = false },

  # merge_with_env_nested
  { b = { c = 10 } }
  & ((fun x => { a = x, b = { c = x } }) 10) == { a = 10, b = { c = 10 } },

  # recursive_records
  { a = 1, b = a + 1, c = b + a } == { a = 1, b = 2, c = 3 },
  {
    f = fun x y =>
      if x == 0 then y else f (x + (-1)) (y + 1)
  }.f
    5
    5 == 10,
  let with_res = fun res =>
    {
      f = fun x =>
        if x == 0 then
          res
        else
          g x,
      g = fun y => f (y + (-1))
    }.f
      10
  in
  with_res "done" == "done",

  # piecewise signatures
  {
    foo : Number,
    bar = 3,
    foo = 5
  }.foo == 5,
  {
    foo : Number,
    foo = 1,
    bar : Number = foo,
  }.bar == 1,
  # This form will probably be deprecated, because using a type annotation
  # outside of a statically typed block is confusing (will the body of `foo` be
  # typechecked? the current answer is no).
  # 
  # The correct form one line after, with a `|`.
  #
  # However, we keep this test until the deprecation and eventual removal to
  # make sure we don't break backward compatibility until then
  let { foo : Number } = { foo = 1 } in foo == 1,
  let { foo | Number } = { foo = 1 } in foo == 1,

  # recursive overriding with common fields
  # regression tests for [#579](https://github.com/tweag/nickel/issues/579)
  # and [#583](https://github.com/tweag/nickel/issues/583)
  ({ foo.bar = foo.baz, foo.baz | default = 2 } & { foo.baz = 1 }).foo.bar == 1,
  let Name = fun b label name => if b then "hijack" else std.contract.blame label in
  let Config = {
    b | Bool,
    name | Name b,
  }
  in
  let data | Config = {
      b = true,
      name = "name",
    }
    in
  data.name == "hijack",

  # recursive overriding with dictionaries
  # regression tests for [#892](https://github.com/tweag/nickel/issues/892)
  (({ a = 1, b = a } | { _ | Number }) & { a | force = 2 }).b == 2,
  (
    {
      b = { foo = c.foo },
      c = {}
    }
      | {
        _ | {
          foo | default = 0
        }
      }
  ).b.foo == 0,

  # regression test for [#1161](https://github.com/tweag/nickel/issues/1161)
  (std.record.map (std.function.const std.function.id) { a = 1, b = a }).b == 1,

  # regression test for [#1224](https://github.com/tweag/nickel/issues/1224)
  std.record.fields ({} | { field | optional = "value" }) == ["field"],

  # check that record type don't propagate through merging
  ({ foo = "a" } | { _ : String })
  & { foo | force = 1 }
  & { bar = false } == { foo = 1, bar = false },

  # check that metadata keyword can be used as record fields without quoting
  let r = {
    optional = true,
    default = false,
    doc = "hello",
    priority = 0,
    force = "no"
  }
  in
  (
    r.optional
    && !r.default
    && r.doc == "hello"
    && r.priority == 0
    && r.force == "no"
  )
]
|> std.test.assert_all
