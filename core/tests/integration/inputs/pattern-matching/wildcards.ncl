# test.type = 'pass'


[
  {foo = 1, bar = 2} |> match {
    {foo = _, baz = _} => false,
    {foo = _, bar = _} => true,
  },

  'Some ('Tag true) |> match {
    'None ('Tag x) => !x,
    'Some ('Other x) => !x,
    'Some ('Tag _) => true,
  },

  'Point {x = 1, y = 2, z = 3} |> match {
    'Line _ => false,
    'Plane {u, v} => false,
    'Volume {u, v, w} => false,
    'Point {x} => false,
    'Point _ => true,
  },
]
|> std.test.assert_all
