# test.type = 'pass'
let Assert = std.test.Assert in

let Y | ((Number -> Number) -> Number -> Number) -> Number -> Number = fun f => (fun x => f (x x)) (fun x => f (x x)) in
let dec : Number -> Number = fun x => x + (-1) in
let or : Bool -> Bool -> Bool = fun x => fun y => if x then x else y in
let fibo : Number -> Number = Y (fun fibo =>
  (fun x => if or (x == 0) (dec x == 0) then 1 else (fibo (dec x)) + (fibo (dec (dec x))))) in
let val : Number = 4 in

(fibo val == 5 | Assert)
