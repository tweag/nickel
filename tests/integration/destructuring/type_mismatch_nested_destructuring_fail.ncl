# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Number'
# found = 'String'
(let { a : { b : Number } = { b }} = { a = { b = "no" }} in
b) : _
