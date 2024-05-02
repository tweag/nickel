# test.type = 'pass'
let {a, ..} = {a=1, b=2} in
a == 1
