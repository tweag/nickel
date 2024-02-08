# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::ArrowTypeMismatch'
#
# [test.metadata.expectation.cause]
# error = 'TypecheckError::VarLevelMismatch'
#
# [test.metadata.expectation.cause.expectation]
# type_var = 'b'
(
  let eval : forall a. (forall b. b -> b) -> a -> a = fun f x => f x
    in
  # because g isn't annotated, it doesn't get a polymorphic type, but a
  # monomorphic _a -> _a
  let g = fun x => x
  in eval g
) : _

