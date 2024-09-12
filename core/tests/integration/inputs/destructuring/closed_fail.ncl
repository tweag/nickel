# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::FailedDestructuring'
let {a} = {a=1, b=2}
in a == 1
