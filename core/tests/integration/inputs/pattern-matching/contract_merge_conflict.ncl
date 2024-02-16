# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::NonMergeableTerms'
{foo.bar | default = 5} |> match {
  {foo={bar ? 6}} => bar,
}
