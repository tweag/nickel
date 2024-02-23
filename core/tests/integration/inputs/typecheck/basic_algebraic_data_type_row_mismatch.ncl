# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::EnumRowMismatch'
(
  let x : [| 'Foo Number |] = 'Foo 5 in
  let y : [| 'Foo Bool |] = 'Foo true in
  # force x and y to have the same type
  if true then x else y
) : _
