# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Bool'
# inferred = 'Number'
{
  f = fun x => if x == 0 then false else 1 + (f (x + (-1)))
} : { f : Number -> Number }
