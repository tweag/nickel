# test.type = 'pass'


[
  let this-isn't-invalid = true in this-isn't-invalid,
  let ___multi_underscore_start = true in ___multi_underscore_start,
]
|> std.test.assert_all
