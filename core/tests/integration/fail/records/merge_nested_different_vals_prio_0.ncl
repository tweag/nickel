# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MergeIncompatibleArgs'
({ a.b | priority 0 = 1, a = { b = 2 }}).a.b