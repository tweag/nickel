# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::UnboundIdentifier'
#
# [test.metadata.expectation]
# identifier = 'foo'
let x = "foo" 
in { "%{x}" = 1, bar = foo }.bar
