# test.type = 'pass'

# pattern matching on pure enum is special-cased for performance reasons, using
# a hashmap. The presence of a wildcard pattern has to be handled differently
# that for normal match expressions, which are just testing all patterns in
# order.
#
# This test ensure it correctly handle a wildcard being used before the end of
# the match expression, which was mishandled previously (branches after the
# wildcard would be selected if they matched).


[
  'Foo |> match {
    'A => false,
    'B => false,
    'C => false,
    _ => true,
    'Foo => false,
  },
  # check for off-by-one error when truncating patterns coming after the
  # wildcard
  'Foo |> match {
    'A => false,
    'B => false,
    'C => false,
    'Foo => true,
    _ => false,
  },
]
|> std.test.assert_all
