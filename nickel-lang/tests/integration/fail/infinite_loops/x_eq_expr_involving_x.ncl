# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::InfiniteRecursion'
{ x = y + z, y = z + x, z = 1 }.x