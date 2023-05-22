# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MergeIncompatibleArgs'
{
  foo.bar | default = false,
  foo.bar | default = true,
}.foo.bar
