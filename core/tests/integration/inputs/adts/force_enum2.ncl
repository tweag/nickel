# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::NAryPrimopTypeError'

# Check that force correctly deeply evaluates the content of an enum variant
%force% ('Foo { bar = 5 + "a" })
