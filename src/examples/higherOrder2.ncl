let f = fun g => Assume(Num, g true) in
let id = fun x => x in
let id2 = fun y => id y in
id2 (f id)
