# test.type = 'pass'
let Assert = std.test.Assert in

(let plus : Number -> Number -> Number = fun x => fun y => x + y in
 plus (54 : Number) (6 : Number) == 60 | Assert)
