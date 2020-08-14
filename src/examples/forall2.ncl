let f = Promise(forall a. (forall b. b -> b) -> a -> a, fun f x => f x) in
Promise(Num, (f Promise(forall b. b -> b, fun x => x)) 3)
