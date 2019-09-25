let f = fun z => z + 3 in
let check = fun y => Assume(Bool, y) in
check (f 4)
