# test.type = 'pass'


[
  'Foo true & 'Foo true == 'Foo true,

  # Checking that environments and closurization are properly handled
  let z = 1 in (let x = 1 + 1 in 'Bar x) & (let y = 1 + z in 'Bar y) == 'Bar 2,
  'Baz { x = 1 } & 'Baz { y = 1 } == 'Baz { x = 1, y = 1 },

  # adt merging plus overriding
  { one = 'Qux two, two = { a = 1 } }
  & { two.b = 1, one = 'Qux {c = 0} }
  == { one = 'Qux { a = 1, b = 1, c = 0 }, two = { a = 1, b = 1 } },
]
|> std.test.assert_all
