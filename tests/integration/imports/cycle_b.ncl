let x = import "cycle.ncl" in
let y = import "cycle_c.ncl" in
{a = x.a, b = y}
