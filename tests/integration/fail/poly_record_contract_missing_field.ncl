# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let f | forall r. { x : Number, y : Number; r } -> { x : Number, y : Number; r} =
  fun r => r
in f { x = 1 }
