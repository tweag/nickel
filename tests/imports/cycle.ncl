
let x = import "cycle_b.ncl" in {a = 1, b = x.a}
