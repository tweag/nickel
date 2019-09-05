let Y = (fun f => (fun x => f (x x)) (fun x => f (x x))) in
let ite = (fun b t e => b t e) in
let t = (fun x y => x) in 
let f = (fun x y => y) in 
let g = Y (fun g => (fun x => ite x (g f) 4)) in
g t 