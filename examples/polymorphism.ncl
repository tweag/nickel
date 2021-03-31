// First projection, statically typed
let f : forall a b. a -> b ->  a = fun x y => x in
// Evaluation function, statically typed
let ev : forall a b. (a -> b) -> a -> b = fun f x => f x in
let id : forall a. a -> a = fun x => x in
(ev id 5 == 5 : Bool)
