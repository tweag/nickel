# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'String'
# inferred = 'Number'
(
  let x = 1 + 2 
  in let f = fun x => x ++ "a" 
  in f x
) : Number
