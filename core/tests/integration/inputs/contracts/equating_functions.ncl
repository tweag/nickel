# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::IncomparableValues'
let g = fun x => x + 1 in
g == g
