# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Bool'
# found = 'Number'
{ a = 1, b : Bool = a } : { a : Number, b : Bool }