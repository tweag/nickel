# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::NAryPrimopTypeError'

# Check that deep_seq correctly deeply evaluates the content of an enum variant
%deep_seq% ('Foo { bar = 5 + "a" }) null
