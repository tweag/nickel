# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'r'
# found = "[| 'bli |]"
let f : forall r. [| 'blo, 'ble; r |] -> Number =
  match { 'blo => 1, 'ble => 2, 'bli => 3 }
in f