# test.type = 'pass'
(
  let y = 'Foo (1 + 1) in
  let 'Foo x = y in
  x + 1 == 3
) : _
