# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
"a" | std.contract.not (std.contract.any_of [Number, String])
