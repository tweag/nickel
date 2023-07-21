# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::ArrowTypeMismatch'
#
# [test.metadata.expectation.sub_error]
# error = 'TypecheckError::VarLevelMismatch'
#
# [test.metadata.expectation.sub_error.expectation]
# type_var = 'b'
(let eval : forall a. (forall b. b -> b) -> a -> a = fun f x => f x
# because g isn't annotated, it doesn't get a polymorphic type, but a
# monomorphic _a -> _a
in let g = fun x => x
in eval g) : _
