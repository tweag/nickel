# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::NonExhaustiveMatch'
{foo.bar = 1, baz = 2} |> match {
  {foo = {bar, other}, ..} => null,
  {foo = {bar, ..}, baz, other} => null,
}
