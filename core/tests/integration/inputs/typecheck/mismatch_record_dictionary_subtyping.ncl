# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
# [test.metadata.expectation]
# expected = 'Number'
# inferred = 'String'
let test : {foo : Number, bar : String} = {foo = 5, bar = "test"} in
(std.record.insert "baz" 5 test) : {_ : Number}
