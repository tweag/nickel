# test.type = 'pass'
let baz = 10 in
let x @ {foo ? 10, b = xb, d ? baz = bar, c = xc @ {ca, ..}, ..y} = {a = 2, b = 3, c = {ca = 22, cb = 0}} in
y.a + x.b - xb + x.c.ca - xc.ca - ca + foo + bar == 0
