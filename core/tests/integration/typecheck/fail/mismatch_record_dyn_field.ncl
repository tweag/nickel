# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = '{ bla : _a; _rrows_b }'
# inferred = '{ _ : _c }'
({
  "%{if true then "foo" else "bar"}" = 2,
  "foo" = true,
}."bla") : Number
