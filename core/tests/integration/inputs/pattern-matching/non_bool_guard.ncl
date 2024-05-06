# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::UnaryPrimopTypeError'
{foo = 'Foo 5, bar = 5} |> match {
  {foo = 'Foo x, bar} if x => x,
}
