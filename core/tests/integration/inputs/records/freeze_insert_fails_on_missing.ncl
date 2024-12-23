# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::MissingFieldDef'
#
# [test.metadata.expectation]
# field = 'bar'
((std.record.insert "bar" 1 { foo = bar + 1, bar | optional }) & { bar | force = 2 }).foo
