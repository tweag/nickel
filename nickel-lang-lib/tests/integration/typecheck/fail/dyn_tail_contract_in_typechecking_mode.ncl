# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::ExtraDynTail'
({ a = 1 } | { a : Number ; Dyn }) : { a : Number }