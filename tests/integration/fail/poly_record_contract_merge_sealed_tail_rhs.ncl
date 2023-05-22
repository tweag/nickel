# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::IllegalPolymorphicTailAccess'
let f | forall r. { a : Number; r } -> { a : Number; r } =
  fun r => { a | force = 0 } & r
in f { a | default = 100, b = 1 }