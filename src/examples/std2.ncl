let dyn = fun l => fun t => t in
let num = fun l => fun t => if isNum t then t else blame l in
let bool = fun l => fun t => if isBool t then t else blame l in
let func = fun s => fun t => fun l => fun e => if isFun e then (fun x => t l (e (s l x))) else blame l in


let const = fun x => fun y => x in
let safeAppTwice = fun f => fun y => f (f y) in
let ma = Promise(func (func num num) (func dyn num), safeAppTwice) 
         (Promise(func dyn (func dyn dyn), const) Promise(bool, 1)) 
         Promise(bool, true) 
         in 
Promise(dyn, ma)