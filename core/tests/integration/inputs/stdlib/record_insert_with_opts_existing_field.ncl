# test.type = 'error'
# eval = 'full'
#
# [test.metadata]
# error = 'EvalError::Other'
let record | {foo_opt | optional, ..} = {
  foo = 1,
  bar = "two",
  baz = false,
}
in

std.record.insert_with_opts "foo_opt" true record
