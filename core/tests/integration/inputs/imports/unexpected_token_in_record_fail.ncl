# test.type = 'error'
#
# [test.metadata]
# error = 'ImportError::ParseError'
let x = import "imported/unexpected_token_in_record.ncl"
in "Hello, " ++ x.name