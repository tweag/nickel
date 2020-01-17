let g = Promise(forall a. a -> a, fun y => (fun x => x) y) in
let f = Promise(forall a. a -> a, fun y => g y) in
f