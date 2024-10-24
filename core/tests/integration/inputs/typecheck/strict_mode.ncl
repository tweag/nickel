# test.type = 'error'
# eval = 'typecheck_strict'
#
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Number'
# inferred = 'String'
1 + "foo"
