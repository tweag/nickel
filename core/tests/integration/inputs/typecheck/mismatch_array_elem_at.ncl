# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'b'
# inferred = 'a'
(fun l => %array/at% l 0) : forall a b. (Array a -> b)
