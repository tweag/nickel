# test.type = 'pass'

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
|> std.test.assert_all
