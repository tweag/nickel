# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'

# Check that deep_seq correctly evaluates the content of an enum variant
%deep_seq% ('Foo 5 | [| 'Foo String |]) null
