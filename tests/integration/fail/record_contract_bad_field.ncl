# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let x | { a : Number, b: String } = { a = 1, s = 2 } 
in %deep_seq% x x