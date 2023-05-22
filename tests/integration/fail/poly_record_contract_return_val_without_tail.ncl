# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let f | forall r. { foo : Number; r } -> { foo : Number; r } =
  fun r => { foo = 1 }
in f { foo = 1, other = 2 }