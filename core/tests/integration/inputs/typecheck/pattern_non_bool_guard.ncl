# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Bool'
# inferred = 'Number'
(
  {foo = 1, bar = 2}
  |> match {
    {foo, bar} if 1+1 => foo + bar,
    _ => 0,
  }
) : _
