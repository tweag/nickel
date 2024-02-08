# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Number'
# inferred = 'Dyn'
let x = (1 + 1) in (x + 1 : Number)
