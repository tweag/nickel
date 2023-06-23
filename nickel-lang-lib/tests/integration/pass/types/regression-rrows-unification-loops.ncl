# test.type = 'pass'
let f : forall a b. (a -> b -> b) -> b -> Dyn = fun x b => null in
let _ign = (f (fun result acc => if result.success then acc else result)) : _ in
# we only care about typechecking here
true
