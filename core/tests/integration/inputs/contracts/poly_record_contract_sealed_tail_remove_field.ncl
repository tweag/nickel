# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::IllegalPolymorphicTailAccess'
let remove_x | forall r. { ; r } -> { ; r } = fun r => %record/remove% "x" r in
remove_x { x = 1 }
