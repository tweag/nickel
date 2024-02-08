# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'lib.Contract'
# inferred = 'lib.Contract'
let lib = { Contract = fun label value => value } in
let foo | lib.Contract = null in
let lib = { Contract = fun label value => value } in
foo : lib.Contract
