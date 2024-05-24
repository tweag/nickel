# test.type = 'pass'


[
  5 + 5 == 10,
  5 * 5 == 25,
  1e6 == 1000000,
  1e+3 / 2e-3 == 0.5e6,
]
|> std.test.assert_all
