# test.type = 'pass'

[
  (import "imported/file_without_extension" as 'Nickel) == 1234,
  (import "imported/file_without_extension" as 'Text) |> std.string.is_match "^1200\\+34\\s*$",
  (import "imported/file_with_unknown_extension.tst" as 'Nickel) == 1234,
  (import "imported/file_with_unknown_extension.tst" as 'Text) |> std.string.is_match "^34\\+1200\\s*$",
  (import "imported/empty.yaml" as 'Text) == "",
  (import "imported/two.ncl" as 'Text) |> std.string.is_match "^\\s*\\#",
]
|> std.test.assert_all
