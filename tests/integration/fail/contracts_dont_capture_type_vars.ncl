# test = 'error'
#
# [metadata]
# error = 'TypecheckError::UnboundIdentifier'
# 
# [metadata.expectation]
# identifier = 'a'
forall a b. b -> Array b -> { _ | a }