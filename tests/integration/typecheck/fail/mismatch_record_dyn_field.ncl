# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = '{ bla: _a ; _a }'
# found = '{ _ : _b }'
({
  "%{if true then "foo" else "bar"}" = 2,
  "foo" = true,
}."bla") : Number