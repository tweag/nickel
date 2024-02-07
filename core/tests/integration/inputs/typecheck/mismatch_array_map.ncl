# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Dyn -> _a'
# inferred = 'a -> b'
(fun f l => %map% l f) : forall a b. (a -> b) -> Array Dyn -> b
