# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::RecordRowMismatch'
let x : Number = 1 in
{ include x } : { x : String }
