# test.type = 'pass'
[  
  "a" |> match {
    "e" or "f" or "g" => false,
    "a" or "b" or "c" => true,
    _ => false,
  },

  'Foo (1+1) |> match {
    ('Bar _) or ('Baz _) => false,
    ('Qux x) or ('Foo x) => x == 2,
    _ => false,
  },

  [1, {field = 'Foo 5}, 2] |> match {
    [_, {field = 'Bar _} or {field = 'Baz _}, _] => false,
    [_, {field = 'Bar _} or {field = 'Foo _}, _] => true,
    _ => false,
  },

  {some = "data"} |> match {
    x if std.is_number x || std.is_string x => false,
    {..} or [..] => true,
    _ => false,
  },

  {field = 'Marked} |> match {
    {field = x} or {data = x} if x == 'Unmarked => false,
    {data = x} or {field = x} if x == 'Marked => true,
    _ => false,
  },

  'Foo 1 |> match {
    ('Foo or) or ('Baz or) => or == 1,
    _ => false,
  },

  'Baz |> match {
    'Foo or 'Bar or 'Baz or 'Qux => true,
    _ => false,
  },
]
|> std.test.assert_all
