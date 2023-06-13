# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MergeIncompatibleArgs'
({ foo.bar | priority -10 = false, foo.bar | priority -10 = true }).foo.bar