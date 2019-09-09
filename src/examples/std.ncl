let num = fun l => fun t => if isNum t then t else blame NotNum in
let bool = fun l => fun t => if isBool t then t else blame NotBool in
let func = fun s => fun t => fun l => fun e => if isFun e then (fun x => t l (e (s l x))) else blame NotFun in


let safePlus = fun x => fun y => num 1 (+ (num 2 x) (num 3 y)) in
let const = fun x => fun y => if isNum y then y else 2 in
let safeAppTwice = fun f => fun y => (func num num 1000 f) ((func bool num 1000 f)  y) in

safeAppTwice (const 3) true