# test.type = 'pass'

let { record, .. } = std in

[
  # record.fields
  std.record.fields { a = 1, b = 2, c | optional } == ["a", "b"],
  std.record.fields ({ a = 1, b = 2 } | { a, b, c | optional }) == ["a", "b"],
  std.record.fields { a = 1, b = 2, c | optional = 1 } == ["a", "b", "c"],
  std.record.fields_with_opts { a = 1, b = 2, c | optional } == ["a", "b", "c"],
  std.record.fields_with_opts ({ a = 1, b = 2 } | { a, b, c | optional }) == ["a", "b", "c"],

  # record.insert
  std.record.insert "a" 1 { a | optional } == { a = 1 },

  # record.remove
  std.record.remove_with_opts "a" { a | optional } == {},

  # record.has_field
  std.record.has_field "c" { a = 1, b = 2, c | optional } == false,
  std.record.has_field "c" ({ a = 1, b = 2 } | { a, b, c | optional }) == false,
  std.record.has_field "c" { a = 1, b = 2, c | optional = 1 } == true,
  std.record.has_field_with_opts "c" { a = 1, b = 2, c | optional } == true,
  std.record.has_field_with_opts "c" ({ a = 1, b = 2 } | { a, b, c | optional }) == true,
]
|> std.test.assert_all
