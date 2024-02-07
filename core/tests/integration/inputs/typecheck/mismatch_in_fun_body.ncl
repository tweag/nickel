# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Number'
# inferred = 'Bool'
let f : Bool -> Number = 
  fun x => 
    if x then x + 1 else 34 
in f false
