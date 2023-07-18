# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'a'
# found = '_a'
(fun x => let y : forall a. a = x in y) : _
