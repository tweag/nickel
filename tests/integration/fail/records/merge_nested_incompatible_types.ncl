# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MergeIncompatibleArgs'
({ a.b = {} } & { a.b.c = [] } & { a.b.c = {}}).a.b.c