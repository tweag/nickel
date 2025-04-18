# test.type = 'pass'

let x = 1, y = false, z = "string", u = {v = 1, depd = v + 1} in
[
  {include x} == {x = 1},
  {include x, include y, include z} == {include [x, y, z]},
  {include x, include [y, z]} == {include [x, y], include z},
  {include [x, y], other = []} == {x = 1, y = false, other = []},

  # merging
  {include x, depd = x + 1} == {x = 1, depd = 2},
  {include x, depd = x + 1} & {x | force = 2} == {x = 2, depd = 3},
  {include u} & {u.v | force = 2} == {u = {v = 2, depd = 3}},

  # include on nested fields,,
  {
    include x,
    y = "redefined",
    lvl1 = true,
    sub = {
      include [x, y, lvl1],
    }
  } == {
    x = 1,
    y = "redefined",
    lvl1 = true,
    sub = {
      x = 1,
      y = "redefined",
      lvl1 = true
    }
  },
]
|> std.test.assert_all
