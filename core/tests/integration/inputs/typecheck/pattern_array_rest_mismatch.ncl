# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Number'
# inferred = 'String'
["hello", "world"] |> match {
    [] => 0,
    [x] => 1,
    [x, ..rest] => std.array.at 0 rest,
 } : _
