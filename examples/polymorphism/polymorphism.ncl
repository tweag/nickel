// First projection, statically typed
let fst : forall a b. a -> b -> a = fun x y => x in
// Evaluation function, statically typed
let ev : forall a b. (a -> b) -> a -> b = fun f x => f x in
let id : forall a. a -> a = fun x => x in
(ev id (fst 5 10) == 5 : Bool)
