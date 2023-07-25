# test.type = 'pass'
# eval = 'typecheck'
let co_eval : ((forall a. a -> a) -> Number) -> Number = (
    (
      fun eval =>
        let id : forall b. b -> b = fun y => y
          in eval id
    )
  )
  in
(co_eval (fun id => id 3)) : _

