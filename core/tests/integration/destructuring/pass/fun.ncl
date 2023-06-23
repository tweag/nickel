# test.type = 'pass'
let f = fun x@{a, b} {c=d, ..} => a - x.a + b - x.b + d in
f {a=1, b=2} {c=3, d=4} == 3
