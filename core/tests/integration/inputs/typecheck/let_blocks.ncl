# test.type = 'pass'
let typecheck = [
  (let
    x = 1,
    y = 2,
  in x + y) : _,

  (let rec
    x = 1,
    y = x,
  in x + y) : _,

  (let x = 1 in
    let
      x = "hi",
      y = x,
      z = 2,
    in y + z) : _,

  (let x = "hi" in
    let rec
      x = 1,
      y = x,
      z = 2,
    in y + z) : _,
]
in
true
