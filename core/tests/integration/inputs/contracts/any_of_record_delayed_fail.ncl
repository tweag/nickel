# test.type = 'error'
# eval = 'full'
#
# [test.metadata]
# error = 'EvalError::BlameError'
{ foo = 1, bar = 2, qux = true} | std.contract.any_of [
  { foo | Number, bar | Number },
  { foo | String, bar | Number, qux | Bool }
]
