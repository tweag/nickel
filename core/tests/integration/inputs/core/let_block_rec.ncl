# test.type = 'pass'
let rec
  b = a,
  a = 2,
  c = b
in
[
  a == 2,
  b == 2,
  c == 2,
]
|> std.test.assert_all
