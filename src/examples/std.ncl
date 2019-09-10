let num = fun l => fun t => if isNum t then t else blame l in
let bool = fun l => fun t => if isBool t then t else blame l in
let func = fun s => fun t => fun l => fun e => if isFun e then (fun x => t l (e (s l x))) else blame l in


let safePlus = fun x => fun y => Promise(num, Promise(num, x) + Promise(num, y)) in
let const = fun x => fun y => if isNum y then y else 2 in
let safeAppTwice = fun f => fun y => Promise(func num num, f) ( Promise(func bool num, f)  y) in

safeAppTwice (const 3) true