# test.type = 'pass'
let { check, Assert, .. } = import "../lib/assert.ncl" in
let ConstantTrue = fun _label value => std.seq value true in
[
  let foo | ConstantTrue | Bool = "not a bool" in
  foo,

  {
    foo
      | ConstantTrue
      | Bool
      = "still not a bool"
  }.foo
]
|> check