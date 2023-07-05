# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::ExtraRow'
#
# [test.metadata.expectation]
# ident = 'bli'
(match {
  'bla => 1,
  'ble => 2,
  'bli => 3,
}) : [| 'bla, 'ble |] -> Number