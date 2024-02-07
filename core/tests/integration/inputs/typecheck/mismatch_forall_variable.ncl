# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'a'
# inferred = 'b'
let f : forall a. (forall b. a -> b -> a) =
  fun x y => y
in f
