# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MergeIncompatibleArgs'
({ a = 1 } & { a = 2 }).a
