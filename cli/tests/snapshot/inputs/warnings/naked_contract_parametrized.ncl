# capture = 'stderr'
# command = ['eval']
let C = fun arg label value => value in
[
  1 | C "hi",
  [1, 2] | Array (C "hi")
]
