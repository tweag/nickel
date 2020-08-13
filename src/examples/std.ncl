let safePlus = fun x y => Promise(Num, Promise(Num, x) + Promise(Num, y)) in
let const = fun x y => if isNum y then y else 2 in
let safeAppTwice = fun f y => Promise(Num -> Num, f) ( Promise(Bool -> Num, f)  y) in
safeAppTwice (const 3) true
