# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Number'
# inferred = 'Bool'
match { 'foo => 3, 'bar => true } 'bar : Number
