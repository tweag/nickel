# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let foo | (Dyn -> Dyn) -> Dyn = fun x y => x y in foo null null
