# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let x | { a : Number, s: String } = { s = "yes" } 
in %deep_seq% x x 
