# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let x | { a : Number, s : String } = { a = 1, s = "a", extra = 1 }
in %deep_seq% x x
