# test.type = 'pass'
let rec
  a = 2,
  { b } = { b = a }
in
b == 2
