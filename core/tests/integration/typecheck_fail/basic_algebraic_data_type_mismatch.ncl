# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Number'
# found = 'Bool'
let force_type_equality : forall a. a -> a -> Dyn = fun _x _y => null in
(
  force_type_equality ('Foo..(5)) ('Foo..(true))
) : _
