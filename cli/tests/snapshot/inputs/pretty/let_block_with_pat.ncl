# capture = 'stdout'
# command = ['pprint-ast']
let
  a = 1,
  d@{ b, c } = { b = "b", c = 2 },
in a + d.c
