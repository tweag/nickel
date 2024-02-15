# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::ArrowTypeMismatch'
#
# [test.metadata.expectation.cause]
# error = 'TypecheckError::MissingRow'
#
# [test.metadata.expectation.cause.expectation]
# ident = 'Other'
match {
    'Foo => null,
    'Bar => null,
    'Baz => null,
  } : [| 'Foo, 'Bar, 'Baz, 'Other |] -> Dyn
