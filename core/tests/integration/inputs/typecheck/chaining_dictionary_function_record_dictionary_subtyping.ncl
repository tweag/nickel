# test.type = 'error'
# [test.metadata]
# error = 'EvalError::BlameError'
let result : _ =
  {foo = 1, bar = 2}
  |> std.record.insert "baz" 5 x
  |> std.record.get "foo" y
  |> std.record.remove "foo" y
  |> std.record.get "foo" z
in
result
