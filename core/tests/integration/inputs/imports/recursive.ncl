# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::InfiniteRecursion'
let x = import "./recursive.ncl" in x
