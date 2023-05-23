# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'p'
# found = "[| 'blo ; _a |]"
let f : forall r. (forall p. [| 'blo, 'ble ; r |] -> [| 'bla, 'bli; p |]) =
  match { 'blo => 'bla, 'ble => 'bli, _ => 'blo } 
in f 'bli