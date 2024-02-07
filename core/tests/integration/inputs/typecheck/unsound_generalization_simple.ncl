# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::VarLevelMismatch'
#
# [test.metadata.expectation]
# type_var = 'a'
(fun x => let y : forall a. a = x in y) : _

