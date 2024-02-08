# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MergeIncompatibleArgs'
({ a | force = false } & { a | force = true }).a