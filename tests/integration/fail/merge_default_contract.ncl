# test = 'error'
#
# [metadata]
# error = 'EvalError::BlameError'
({ a = 2 } & { b | Number } & { b | default = true }).b