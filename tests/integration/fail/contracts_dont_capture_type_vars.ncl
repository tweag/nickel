# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::UnboundIdentifier'
# 
# [test.metadata.expectation]
# identifier = 'a'
forall a b. b -> Array b -> { _ | a }