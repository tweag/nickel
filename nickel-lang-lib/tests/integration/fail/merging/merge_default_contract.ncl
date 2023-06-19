# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
({ a = 2 } & { b | Number } & { b | default = true }).b