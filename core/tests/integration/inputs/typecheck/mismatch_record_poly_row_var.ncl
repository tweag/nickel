# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::ForallParametricityViolation'
#
# [test.metadata.expectation]
# tail = '{ ; r }'
# violating_type = '{ x : String }'
let f : forall r. { x : String } -> { ; r } =
  fun x => x
in f { x  = 1, y = 2 } 
