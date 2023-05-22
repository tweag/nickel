# test.type = 'skip'
let x = import "circular_imports.ncl" in
let y = import "circular_imports2.ncl" in
{a = x.a, b = y}
