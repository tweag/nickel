# test = 'error'
#
# [metadata]
# error = "EvalError::BlameError"
let throw | (fun l _v => %blame% l) = null in 
false || true && throw
