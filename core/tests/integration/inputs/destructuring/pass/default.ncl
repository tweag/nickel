# test.type = 'pass'
let {a ? 1, b} = {b=2} in
a + b == 3
