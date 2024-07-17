# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
true | std.contract.any_of [Number, String]
