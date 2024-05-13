# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::OrPatternVarsMismatch'
#
# [test.metadata.expectation]
# var = 'y'
{data = 'Foo 5} |> match {
 {data = 'Foo x} or {field = y @ 'Bar x} => true,
}
