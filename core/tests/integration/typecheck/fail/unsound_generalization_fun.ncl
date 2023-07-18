# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'c'
# found = '_a'
(fun x => let y : forall c d. c -> d = fun z => x z in y) : _
