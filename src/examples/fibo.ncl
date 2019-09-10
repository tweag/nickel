let dyn = fun l => fun t => t in
let num = fun l => fun t => if isNum t then t else blame l in
let bool = fun l => fun t => if isBool t then t else blame l in
let func = fun s => fun t => fun l => fun e => if isFun e then (fun x => t l (e (s l x))) else blame l in

let Y = (fun f => (fun x => f (x x)) (fun x => f (x x))) in
let dec = Promise(func num num, fun x => + x (-1)) in
let or = Promise(func bool (func bool bool), fun x => fun y => if x then x else y) in

let fibo = Promise(func num num, Y (fun fibo =>
    (fun x => if or (isZero x) (isZero (dec x)) then 1 else + (fibo (dec x)) (fibo (dec (dec x)))))) in
let val = Promise(num, 7) in
fibo val