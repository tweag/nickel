# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::UnboundIdentifier'
#
# [test.metadata.expectation]
# identifier = 'baz'
(
  {foo = 1, bar = 2}
  |> match {
    {foo, bar} if foo+bar+baz == 0 => foo + bar,
    _ => 0,
  }
) : _
