# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::ExtraRow'
#
# [test.metadata.expectation]
# ident = 'Bar'
(let y : [| 'Foo Number, 'Bar String |] = 'Foo 5 in let 'Foo x = y in x) : _
