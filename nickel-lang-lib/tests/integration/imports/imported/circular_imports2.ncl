# test.type = 'skip'
let _x = import "circular_imports.ncl" in
let _y = import "circular_imports1.ncl" in
0
