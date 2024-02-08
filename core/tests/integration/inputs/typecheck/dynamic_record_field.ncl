# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = '{ foo : Number }'
# inferred = '{ _ : _a }'
let x = "foo"
in { "%{x}" = 1 } : { foo : Number }
