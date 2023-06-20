# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let r = { a = 2 } & { a | Bool } in r.a
