# test.type = 'pass'
let x @ {..} = {a=1, b=2} in
x.a + x.b == 3
