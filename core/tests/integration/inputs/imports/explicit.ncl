# test.type = 'pass'

# Note: cannot use repeated file, as it gets cached with the first encountered type
[
  (import 'Nickel "imported/file_without_extension") == 1234,
  (import 'Raw    "imported/file_with_unknown_extension.tst") == "34+1200\n",
  (import 'Raw    "imported/empty.yaml") == "",
  (import 'Raw    "imported/two.ncl") == "# test.type = 'skip'\n1 + 1 : Number\n",
]
|> std.test.assert_all
