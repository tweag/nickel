let f a b@{x, w=y} = a + b.x - x + b.w - y in f 1 {x=2, w=3} == 1
