# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Array _a'
# inferred = 'String'
"hello" |> match {
    [] => 0,
    [x] => 1,
    [x, ..] => 2,
 } : _
