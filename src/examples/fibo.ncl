let dyn = fun l => fun t => t in
let num = fun l => fun t => if isNum t then t else l in
let bool = fun l => fun t => if isBool t then t else  l in
let func = fun s => fun t => fun l => fun e => if isFun e then (fun x => t l (e (s l x))) else l in

let Y = (fun f => (fun x => f (x x)) (fun x => f (x x))) in
let dec = func num num (blame label[dec]) (fun x => + x (-1)) in
let or = func bool (func bool bool) (blame label[or]) (fun x => fun y => if x then x else y) in

let fibo = func num num (blame label[fibo]) (Y (fun fibo =>
    (fun x => if or (isZero x) (isZero (dec x)) then 1 else + (fibo (dec x)) (fibo (dec (dec x)))))) in
let val = num (blame label[num_value]) 4 in
fibo val