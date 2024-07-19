# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
{ foo = 1, bar = 2} | std.contract.not {foo, bar}
