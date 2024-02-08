# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let f | forall r. { ; r } -> { x : { y : Number ; r } ; r }
      = fun r => %record_insert% "x" r (%record_insert% "y" r 1) in
f { y = 3 }
