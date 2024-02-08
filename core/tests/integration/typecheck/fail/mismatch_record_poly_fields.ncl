# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'Number'
# inferred = 'Bool'
let f : forall a. { bla : Bool, blo: a, ble: a } -> a =
  fun r => if r.bla then r.blo else r.ble
in (f { bla = true, blo = 1, ble = true } : Number)
