# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Array _a'
# inferred = 'Number'
(let head = fun l => (%array/at% l 0) in (head 10)) : _
