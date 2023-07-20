# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::VariableLevelMismatch'
#
# [test.metadata.expectation]
# type_var = 'a'
(fun x => let y : forall a. a = x in y) : _
