# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::RecordRowConflict'
#
# [test.metadata.expectation]
# row = 'x'
let x : Number = 1 in
{ include x } : { x : String }
