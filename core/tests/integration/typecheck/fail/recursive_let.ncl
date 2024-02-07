# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Number'
# inferred = 'String'
let rec f : Number -> Number =
  fun x => f "hmm"
in null
