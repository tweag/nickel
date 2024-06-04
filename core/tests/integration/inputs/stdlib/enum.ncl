# test.type = 'pass'
let enum = std.enum in

[
  enum.is_enum_tag 'A,
  !(enum.is_enum_tag ('A 'arg)),
  enum.is_enum_variant ('A 'arg),
  !enum.is_enum_variant 'A,

  let enum_round_trip = fun enum_value =>
    enum_value
    |> enum.to_tag_and_arg
    |> enum.from_tag_and_arg
    |> (==) enum_value
  in
  [
    enum_round_trip 'Foo,
    enum_round_trip ('Foo 'arg),
    enum_round_trip ('Foo { value = "hello" }),
  ]
  |> std.test.assert_all,

  let record_round_trip = fun data =>
    data
    |> enum.from_tag_and_arg
    |> enum.to_tag_and_arg
    |> (==) data
  in
  [
    record_round_trip { tag = "Foo" },
    record_round_trip { tag = "Foo", arg = "arg" },
    record_round_trip { tag = "Foo", arg = { value = "hello" } },
  ]
  |> std.test.assert_all,

  'Foo
    |> std.enum.map (fun _ => null)
    |> (==) 'Foo,

  'Foo 2
    |> std.enum.map ((*) 2)
    |> (==) ('Foo 4),
]
|> std.test.assert_all
