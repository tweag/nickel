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
    'Foo x => x ++ "a",
    'Bar y => y + 1,
 } : _
