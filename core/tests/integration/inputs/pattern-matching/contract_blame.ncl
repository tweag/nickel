# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
{foo.bar = 5} |> match {
  {foo={bar | String}} => bar,
}
