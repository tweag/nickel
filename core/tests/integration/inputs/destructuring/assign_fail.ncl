# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::UnboundIdentifier'
#
# [test.metadata.expectation]
# identifier = 'a'
let {a=x} = {a=1}
in a == 1
