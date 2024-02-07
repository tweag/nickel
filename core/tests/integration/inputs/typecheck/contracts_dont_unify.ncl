# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'fun l t => t'
# inferred = 'fun l t => t'
(fun x => x) : (fun l t => t) -> (fun l t => t)
