# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let x | { a : Number, b : { foo : Bool }} = { a = 1, b = {}}
in %deep_seq% x x