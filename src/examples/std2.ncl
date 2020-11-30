let const = fun x y => x in
let safeAppTwice = fun f y => f (f y) in
let ma = (safeAppTwice : (Dyn -> Num) -> Dyn -> Num)
         ((const : Dyn -> Dyn -> Dyn) (1 : Bool))
         (true : Bool)
         in
ma : Dyn
