# test.type = 'skip'
let x = import "two.ncl" in
let y  = import "nested.ncl" in
x + y
