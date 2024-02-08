# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MissingFieldDef'
#
# [test.metadata.expectation]
# field = 'a'
{ a | not_exported, b = a + 1 }.a