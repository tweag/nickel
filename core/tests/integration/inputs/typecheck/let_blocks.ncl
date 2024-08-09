# test.type = 'pass'
(let typecheck = [
  let
    x = 1,
    y = 2,
  in x + y,

  let rec
    x = 1,
    y = x,
  in x + y,

  let x = 1 in
  let
    x = "hi",
    y = x,
    z = 2,
  in y + z,

  let x = "hi" in
  let rec
    x = 1,
    y = x,
    z = 2,
  in y + z,

  let
    { x, y } = { x = 1, y = x },
    z = 2,
  in y + z,
]
in
true): _
