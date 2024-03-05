# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'

# Check that force correctly evaluates the content of an enum variant
%force% ('Foo 5 | [| 'Foo String |])
