# test.type = 'pass'

[
  (import 'Nickel "imported/file_without_extension") == 1234,
  (import 'Raw    "imported/file_without_extension") |> std.string.is_match "^1200\\+34\\s*$",
  (import 'Nickel "imported/file_with_unknown_extension.tst") == 1234,
  (import 'Raw    "imported/file_with_unknown_extension.tst") |> std.string.is_match "^34\\+1200\\s*$",
  (import 'Raw    "imported/empty.yaml") == "",
  (import 'Raw    "imported/two.ncl") |> std.string.is_match "^\\s*\\#",
]
|> std.test.assert_all
