# test.type = 'pass'
[
  let x = { has_number = false } in
  { has_number = x.has_number, test = x.number == 1 }
  |> match {
    { has_number = true, test = true } => "number == 1",
    { has_number = true, test = false } => "number != 1",
    { has_number = false, .. } => true,
  },
  let x = {} in
  [ true, { test = x.number == 1 }]
  |> match {
    [ false, { test = true }] => false,
    [ true, _] => true,
  },
]
|> std.test.assert_all
