# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = "Number"
# inferred = "String"
(let x = 1 in
let rec
  y = x,
  x = "1",
in
y + 1): _
