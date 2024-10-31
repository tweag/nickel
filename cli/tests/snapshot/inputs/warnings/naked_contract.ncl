# capture = 'stderr'
# command = ['eval']
let C = fun label value => value in
[
  1 | C,
  [1, 2] | Array C
]
