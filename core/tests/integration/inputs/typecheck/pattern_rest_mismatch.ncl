# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::ArrowTypeMismatch'
#
# [test.metadata.expectation.cause]
# error = 'TypecheckError::ForallParametricityViolation'
#
# [test.metadata.expectation.cause.expectation]
# tail = '{ ; rows }'
# violating_type = '{ baz : Number; _rrows_a }'
 match {
   {x, y = 'Foo y, ..rest} => y + rest.bar,
   {x, y = 'Bar y, ..rest} => y + rest.baz,
 } : forall a b rows.
       {x: a, y : [| 'Foo Number, 'Bar Number |], bar: Number; rows}
       -> Number
