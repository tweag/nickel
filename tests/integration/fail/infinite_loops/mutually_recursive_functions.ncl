# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::InfiniteRecursion'
{ 
  x = (fun a => a + y) 0,
  y = (fun a => a + x) 0,
}.x