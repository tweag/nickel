# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::TypeError'
{ foo = 'Foo 5, bar = 5 }
|> match {
  { foo = 'Foo x, bar } if x => x,
}
