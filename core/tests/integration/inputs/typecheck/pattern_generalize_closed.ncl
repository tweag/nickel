# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::ArrowTypeMismatch'
# 
# [test.metadata.expectation.cause]
# error = 'TypecheckError::ForallParametricityViolation'
#
# [test.metadata.expectation.cause.expectation]
# tail = '[| ; r |]'
# violating_type = '[|  |]'
match {
    'Foo => null,
    'Bar => null,
    'Baz => null,
  } : forall r. [| 'Foo, 'Bar, 'Baz; r |] -> Dyn
