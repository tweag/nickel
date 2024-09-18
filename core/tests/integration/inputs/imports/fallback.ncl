# test.type = 'pass'

# Testing that importing files with unknown extension interprets them as Nickel source code
[
  (import "imported/file_without_extension") == 1234,
  (import "imported/file_with_unknown_extension.tst") == 1234,
]
|> std.test.assert_all
