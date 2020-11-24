let g : forall a. a -> a = fun y => (fun x => x) y in
let f : forall a. a -> a = fun y => g y in
f
