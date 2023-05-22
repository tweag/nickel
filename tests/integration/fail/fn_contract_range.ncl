# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let id | forall a. a -> a = fun x => false 
in id false && false