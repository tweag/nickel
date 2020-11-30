let f0 : ((forall a. a -> a) -> Num) -> Num = fun f =>
  f (fun y => y : forall b.  b -> b)
in
f0 (fun g => 3)
