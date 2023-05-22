# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::IllegalPolymorphicTailAccess'
let f | forall r. { a : Number; r } -> { a : String; r } =
  fun r => %record_map% r (fun x => %to_str% x)
in f { a = 1, b = 2 }