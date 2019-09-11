let const = fun x => fun y => x in
let safeAppTwice = fun f => fun y => f (f y) in
let ma = Promise((Dyn -> Num) -> Dyn -> Num, safeAppTwice) 
         (Promise(Dyn -> Dyn -> Dyn, const) Promise(Bool, 1)) 
         Promise(Bool, true) 
         in 
Promise(Dyn, ma)