# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'String'
# inferred = 'Number'
match {
    [x] => x ++ "a",
    [x, y] => y + 1,
 } : _
