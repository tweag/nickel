# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'String'
# found = 'Number'
(let x = import "imported/two.ncl" in x) : String