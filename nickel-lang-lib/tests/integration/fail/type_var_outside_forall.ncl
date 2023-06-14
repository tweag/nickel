# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::UnboundIdentifier'
#
# [test.metadata.expectation]
# identifier = 'a'
let f | a -> (forall a. a -> a) =
  fun x => builtin.seq x null
in null