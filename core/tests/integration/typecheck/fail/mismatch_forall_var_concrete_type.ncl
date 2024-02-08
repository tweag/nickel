# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'a'
# inferred = 'Number'
let g : Number -> Number = fun x => x in
let f : forall a. a -> a = fun x => g x in
f
