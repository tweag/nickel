# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::UnboundIdentifier'
#
# [test.metadata.expectation]
# identifier = "x"
(let
  x = 1,
  y = x,
in
true): _
