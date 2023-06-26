# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::IllegalPolymorphicTailAccess'
let f | forall r. { a : Number; r } -> { a : Number; r } =
  fun r => r & { a | force = 0 }
in f { a | default = 100, b = 1 }