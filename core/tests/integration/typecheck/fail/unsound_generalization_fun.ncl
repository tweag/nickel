# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::VarLevelMismatch'
#
# [test.metadata.expectation]
# type_var = 'c'
(fun x => let y : forall c d. c -> d = fun z => x z in y) : _

