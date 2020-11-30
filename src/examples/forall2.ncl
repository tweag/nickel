let f : forall a. (forall b. b -> b) -> a -> a = fun f x => f x in
(f (fun x => x : forall b. b -> b) 3) : Num
