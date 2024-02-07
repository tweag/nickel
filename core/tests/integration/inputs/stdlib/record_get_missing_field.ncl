# test.type = 'error'
# eval = 'full'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let record = {
  foo = 1,
  bar = "two",
  baz = false,
}
in

record
|> std.record.remove "foo"
|> std.record.get "foo"
