# test.type = 'error'
# eval = 'full'
#
# [test.metadata]
# error = 'EvalError::MergeIncompatibleArgs'
{
  foo | default = (fun x => x),
  foo | default = (fun x => x) 1
}