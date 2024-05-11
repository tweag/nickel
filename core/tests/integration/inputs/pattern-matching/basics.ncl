# test.type = 'pass'


[
  {foo = 1, bar = 2} |> match {
    {foo, baz} => foo + baz,
    {foo, bar} => foo + bar,
  } == 3,

  {foo = 1, bar = 2} |> match {
    {foo, baz} => foo + baz,
    {foo, ..rest} => foo + rest.bar,
  } == 3,

  {foo.sub.x = "hello", foo.sub.y = "world", baz.other = 'Right true } |> match {
    {foo = s @ {sub = {x, ..}}, baz = {other = 'Wrong arg}} => if arg then x else s.y,
    {foo = s @ {sub = {x, ..}}, baz = {other = 'Right arg}} => if arg then x else s.y,
  } == "hello",

  {empty = {}, foo = ["a", "b"], bar.baz = 'Tag} |> match {
    {empty = {}, foo, bar = {baz = 'Tog}} => false,
    {empty = {}, foo = {..}, bar = {baz = 'Tag}} => false,
    _ => true,
  },

  'Some ('Tag true) |> match {
    'None ('Tag x) => !x,
    'Some ('Other x) => !x,
    'Some ('Tag x) => x,
  },

  'Point {x = 1, y = 2, z = 3} |> match {
    'Line u => false,
    'Plane {u, v} => false,
    'Volume {u, v, w} => false,
    'Point {x} => false,
    'Point {x, y, z} => true,
    'Point {x, y, ..} => false,
  },

  let rec eval = fun env => match {
    'Literal v => v,
    'Op {op = 'Add, arg1, arg2} => (eval env arg1) + (eval env arg2),
    'App {f, arg} => (eval env f) (eval env arg),
    'Fun {var = 'Var id, body} => fun arg => eval (env & {"%{id}" = arg}) body,
    'Var id => env."%{id}",
  } in
  'App {
    f = 'Fun {
      var = 'Var "x",
      body = 'Op {
        op = 'Add,
        arg1 = 'Literal 5,
        arg2 = 'Var "x"
      }
    },
    arg = 'Literal 6,
  }
  |> eval {}
  |> (==) 11,
]
|> std.test.assert_all
