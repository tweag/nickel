# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MissingFieldDef'
#
# [test.metadata.expectation]
# field = 'something'
{ something | Number, other = something + 1 }.something