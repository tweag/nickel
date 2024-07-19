# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
{ foo = 1, bar = 2, qux } | std.contract.any_of [
  { foo | Number, bar | String },
  { foo | String, bar | Number, baz | Bool }
]
