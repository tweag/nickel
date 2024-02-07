# test.type = 'skip'
let x = import "circular_imports1.ncl" in {a = 1, b = x.a}
