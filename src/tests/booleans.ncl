let ite = (fun b t e => b t e) in
let t = (fun x y => x) in 
let f = (fun x y => y) in 
let g = (fun x => ite x f 4) in 
g t