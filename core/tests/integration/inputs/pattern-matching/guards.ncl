# test.type = 'pass'
[
  'Ok |> match {
    'Ok if false => false,
    'Ok if true => true,
    _ => false,
  },

  'Some "true" |> match {
    'Some x if std.is_number x => false,
    'Some x if std.is_bool x => false,
    'Some x if std.is_string x => x == "true",
    _ => false,
  },

  {
    hello = ["hello"],
    world=["world"]
  }
  |> match {
    {hello, world} if std.array.length hello == 0 => false,
    {hello, universe} if true => false,
    {hello, world}
      if (world |> (@) hello
        |> std.string.join ", ")
        == "hello, world" => true,
    _ => false
  }
]
|> std.test.assert_all
