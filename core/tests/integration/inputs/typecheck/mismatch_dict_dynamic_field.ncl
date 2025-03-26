# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Bool'
# inferred = 'Number'

# Regression test for https://github.com/tweag/nickel/issues/2124
let x = "a" in {"%{x}" = 1, bar = false} : {_ : Bool}
