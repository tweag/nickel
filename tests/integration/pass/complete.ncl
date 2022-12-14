let {Assert, ..} = import "lib/assert.ncl" in

let Y | ((Num -> Num) -> Num -> Num) -> Num -> Num = fun f => (fun x => f (x x)) (fun x => f (x x)) in
let dec : Num -> Num = fun x => x + (-1) in
let or : Bool -> Bool -> Bool = fun x => fun y => if x then x else y in
let fibo : Num -> Num = Y (fun fibo =>
  (fun x => if or (x == 0) (dec x == 0) then 1 else (fibo (dec x)) + (fibo (dec (dec x))))) in
let val : Num = 4 in

(fibo val == 5 | Assert)
