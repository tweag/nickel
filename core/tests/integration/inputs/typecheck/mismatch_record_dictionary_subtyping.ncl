# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::InhomogeneousRecord'
# [test.metadata.expectation]
# row_a = 'Number'
# row_b = 'String'
let test : { foo : Number, bar : String } = { foo = 5, bar = "test" } in
(std.record.insert "baz" 5 test) : { _ : Number }
