let safePlus = fun x => fun y => Promise(Num, Promise(Num, x) + Promise(Num, y)) in
let const = fun x => fun y => if isNum y then y else 2 in
let safeAppTwice = fun f => fun y => Promise(Num -> Num, f) ( Promise(Bool -> Num, f)  y) in

safeAppTwice (const 3) true