# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let f | (forall r. { x : Number; r } -> { ; r })
        -> { x : Number, y : Number } -> { y : Number } =
  fun g r => g r
in f (fun r => r) { x = 1, y = 2 }