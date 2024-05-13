# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::ArrowTypeMismatch'
#
# [test.metadata.expectation.cause]
# error = 'TypecheckError::RecordRowMismatch'
match {
  {field = 'Foo x}
  or {field = 'Bar x}
  or {field = 'Baz x} =>
    null,
}: forall a r. {field: [| 'Foo a, 'Bar a, 'Baz a; r |]} -> Dyn
