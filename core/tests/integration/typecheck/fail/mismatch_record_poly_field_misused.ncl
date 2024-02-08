# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'a'
# inferred = 'Number'

# TODO: the expected/found types above seem wrong here
let f : forall a. (forall r. { bla : Bool, blo: a, ble: a; r } -> a) =
  fun r => if r.bla then (r.blo + 1) else r.ble
in (f { bla = true, blo = 1, ble = 2, blip = 'blip } : Number)
