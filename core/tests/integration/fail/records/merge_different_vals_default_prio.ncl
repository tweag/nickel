# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MergeIncompatibleArgs'
({ a | default = false } & { a | default = true }).a