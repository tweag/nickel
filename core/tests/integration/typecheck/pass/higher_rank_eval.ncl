# test.type = 'pass'
# eval = 'typecheck'
let eval : forall a. (forall b. b -> b) -> a -> a = fun f x => f x
  in
(eval (fun x => x)) : _

