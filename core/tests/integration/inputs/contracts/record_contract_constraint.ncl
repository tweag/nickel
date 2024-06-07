# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let f | forall r. { ; r } -> { x : { y : Number ; r } ; r }
      = fun r => %record/insert% "x" r (%record/insert% "y" r 1) in
f { y = 3 }
