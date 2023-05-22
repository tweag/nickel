# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::FieldMissing'
#
# [test.metadata.expectation]
# field = 'a'
let {a, ..y} = {a=1, b=2} in
y.a
