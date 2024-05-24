# test.type = 'pass'


[
  let {x ? 1, y ? 1, z} = {z = 1} in (x + y + z) == 3,
  let {x ? 1, y ? 1, z} = {x | default = 2, y, z = 1} in (x + y + z) == 4,
  let {inner ? {default = true}} = {inner.nondefault = true} in inner.nondefault,

  {x.e = true} |> match {
    {x = {y ? false, z}} => false,
    {x = {y, e}} => false,
    {x = {y ? false, e}} => true,
    _ => false,
  },
]
|> std.test.assert_all
