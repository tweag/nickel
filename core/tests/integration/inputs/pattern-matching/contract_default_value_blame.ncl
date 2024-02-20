# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
{foo = {}} |> match {
  {foo={bar | String ? 5}} => bar,
}
