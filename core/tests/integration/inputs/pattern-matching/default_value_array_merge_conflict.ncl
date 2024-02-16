# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
{foo.bar | default = []} |> match {
  {foo={bar ? [1]}} => bar,
}
