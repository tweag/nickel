# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::Other'
{ "%{(if false then "foo" else "bar")}" = false, bar = true }.foo