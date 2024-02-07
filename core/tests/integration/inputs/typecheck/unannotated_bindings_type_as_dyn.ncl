# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = '_a -> _b'
# inferred = 'Dyn'
let id = fun x => x
in (id 4 : Number)
