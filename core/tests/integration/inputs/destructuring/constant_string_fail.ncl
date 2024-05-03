# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::NonExhaustiveMatch'
let "ab" = "a" ++ "b" ++ "c" in true
