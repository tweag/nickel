# test.type = 'error'
#
# [test.metadata]
# error = 'ParseError::DuplicateIdentInRecordPattern'
#
# [test.metadata.expectation]
# ident = 'a'
(
  let { a, a, .. } = { a = 1, b = 2 } in
  a : Number
): _
