# test.type = 'error'
# eval = 'full'
#
# [test.metadata]
# error = 'EvalError::FieldMissing'
#
# [test.metadata.expectation]
# field = 'foo_opt'
let record | {foo_opt | optional, ..} = {
  foo = 1,
  bar = "two",
  baz = false,
}
in

std.record.remove "foo_opt" record
