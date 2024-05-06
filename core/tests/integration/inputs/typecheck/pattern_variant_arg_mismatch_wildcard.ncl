# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::EnumRowMismatch'
match {
    'Foo x => let _ = x + 1 in null,
    'Foo y => let _ = y ++ "a" in null,
    'Foo _ => null,
 } : _
