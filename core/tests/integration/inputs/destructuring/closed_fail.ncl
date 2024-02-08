# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let {a} = {a=1, b=2}
in a == 1
