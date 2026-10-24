# test.type = 'pass'

let expected = {
  plus_inf = 'PlusInfinity,
  minus_inf = 'MinusInfinity,
  not_a_number = 'NotANumber,
}
in

[
  import "imported/unrepresentable_float_values.yaml",
  import "imported/unrepresentable_float_values.toml",
]
|> std.array.map ((==) expected)
|> std.test.assert_all
