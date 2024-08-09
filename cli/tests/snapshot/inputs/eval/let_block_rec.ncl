# command = ['eval']
# capture = 'stdout'
let rec
  b = a,
  a = 2,
  c = b
in
c + b
