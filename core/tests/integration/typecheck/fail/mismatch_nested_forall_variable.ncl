# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'forall a. a -> a'
# found = '_b -> _b'
((fun f =>
  let g : forall b. b -> b =
    fun y => y
  in f g) : ((forall a. a -> a) -> Number) -> Number)
  (fun x => 3)