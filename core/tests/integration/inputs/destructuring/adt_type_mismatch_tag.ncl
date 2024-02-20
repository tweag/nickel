# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::ExtraRow'
#
# [test.metadata.expectation]
# ident = 'Bar'
(let 'Foo x = 'Bar 5 in x) : _
