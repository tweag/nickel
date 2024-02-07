# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = '_a -> _b'
# inferred = 'Dyn'
let f : _ -> _ =  fun x => x + 1 in 
let g : Number = f 0 in 
g
