# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = '_a -> _b'
# inferred = 'Dyn'
(fun id => { a : Number = id 4, b : Bool = id true }) (fun x => x)
