# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let f | (forall r. { x : Number; r } -> { x : Number; r })
        -> { x : String, y : Number } -> { x : Number, y: Number } =
  fun g r => g r 
in (f (fun r => r) { x = "", y = 1 }).x
