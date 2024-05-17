# test = 'pass'

# First projection, statically typed
let first : forall a b. a -> b -> a = fun x y => x in
# Evaluation function, statically typed
let eval : forall a b. (a -> b) -> a -> b = fun f x => f x in
let id : forall a. a -> a = fun x => x in
(eval id (first 5 10) == 5 : Bool)
