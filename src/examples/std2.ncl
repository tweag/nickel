let dyn = fun l => fun t => t in
let num = fun l => fun t => if isNum t then t else blame l in
let bool = fun l => fun t => if isBool t then t else blame l in
let func = fun s => fun t => fun l => fun e => if isFun e then (fun x => t l (e (s l x))) else blame l in


let const = fun x => fun y => x in
let safeAppTwice = fun f => fun y => f (f y) in
let ma = (func (func num num) (func dyn num) label[safeAppTwice] safeAppTwice) 
         ((func dyn (func dyn dyn) label[const] const) (bool label[n1] 1)) 
         (bool label[ltrue] true) 
         in 
dyn label[ma] ma