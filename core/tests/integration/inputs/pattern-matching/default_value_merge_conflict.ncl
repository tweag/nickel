# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MergeIncompatibleArgs'
{foo.bar | default = 5} |> match {
  {foo={bar ? 6}} => bar,
}
