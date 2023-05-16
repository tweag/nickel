# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::MissingRow'
#
# [test.metadata.expectation]
# ident = 'b'
(
  let { a, b } = { a = 1, c = 2 } in
  a: Number
): _
