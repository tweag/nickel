# test.type = 'error'
# eval = 'full'
#
# [test.metadata]
# error = 'EvalError::BlameError'

# Check that contracts in enums are evaluated
'Foo { bar = "hi" } | [| 'Foo { bar | Number } |]
