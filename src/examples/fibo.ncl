let Y = Assume(((Num -> Num) -> Num -> Num) -> Num -> Num, fun f => (fun x => f (x x)) (fun x => f (x x))) in
let dec : Num -> Num = fun x => x + (-1) in
let or = Assume(Bool -> Bool -> Bool, fun x y => if x then x else y) in

let fibo : Num -> Num = Y (fun fibo =>
    (fun x => if or (isZero x) (isZero (dec x)) then 1 else (fibo (dec x)) + (fibo (dec (dec x))))) in
let val : Num = 6 in
fibo val
