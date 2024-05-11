# test.type = 'pass'
[
  [] |> match {
    [x] => false,
    [x, ..] => false,
    [x, y] => false,
    [] => true,
    _ => false,
  },

  [] |> match {
    [x] => false,
    [x, ..] => false,
    [x, y] => false,
    [..rest] => rest == [],
    _ => false,
  },

  [0, 1, 2] |> match {
    [x] => false,
    [x, y, z] => x == 0 && y == 1 && z == 2,
    [x, ..] => false,
    _ => false,
  },

  [0, 1, 2] |> match {
    [x] => false,
    [x, y] => false,
    [x, y, z, a] => false,
    [x, y, z, a, ..] => false,
    [x, y, z, ..rest] => x == 0 && y == 1 && z == 2 && rest == [],
    _ => false,
  },

  ["a", "b", "c", "d"] |> match {
    [x, y, ..rest] => x == "a" && y == "b" && rest == ["c", "d"],
    _ => false,
  },

  [[0,1], [1,1], [1,0]] |> match {
    [[x, y], [1, 1, ..], [a, b]] => x == 0 && y == 1 && a == 1 && b == 0,
    _ => false,
  },

  ['Hello "world", 'Goodbye "universe"] |> match {
    ['Hello, 'Goodbye] => false,
    ['Hello x, 'Goodbye y, z] => false,
    ['Hello world, 'Goodbye universe] => world == "world" && universe == "universe",
    _ => false,
  }
]
|> std.test.assert_all
