let safePlus = fun x y => ((x : Num) + (y : Num) : Num) in
let const = fun x y => if isNum y then y else 2 in
let safeAppTwice = fun f y => (f : Num -> Num) ((f : Bool -> Num) y) in
safeAppTwice (const 3) true
