# test.type = 'pass'
let Assert = std.test.Assert in
((import "../lib/imported.ncl") 3 == 3 | Assert)
