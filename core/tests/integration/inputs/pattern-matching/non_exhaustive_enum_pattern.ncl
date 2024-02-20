# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::NonExhaustiveMatch'
{foo = 'Foo 5, bar = 5} |> match {
  {foo = 'Bar x, bar} => x,
  {foo = 'Baz x, ..rest} => x,
}
