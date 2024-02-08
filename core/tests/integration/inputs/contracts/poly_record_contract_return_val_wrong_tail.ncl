# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let f | forall r r'. { a : Number; r }
        -> { a: Number; r' }
        -> { a: Number; r } =
  fun r r' => r'
in f { a = 1, b = "yes" } { a = 1, b = "no" }