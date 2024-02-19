# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::ArrowTypeMismatch'
#
# [test.metadata.expectation.cause]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation.cause.expectation]
# expected = 'a'
# inferred = "[| 'Foo, 'Bar, 'Baz; _erows_a |]"
match {
    'Foo => null,
    'Bar => null,
    'Baz => null,
    _ => null,
  # As long as there's a tag in one of the patterns, the type of the match is
  # inferred to be an enum type (also, inferring a fully polymorphic type as
  # below would break parametricity)
  } : forall a. a -> Dyn
