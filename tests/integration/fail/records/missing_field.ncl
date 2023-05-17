# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MissingFieldDef'
#
# [test.metadata.expectation]
# field = 'foo'
{ foo, bar = foo + 1 }.foo