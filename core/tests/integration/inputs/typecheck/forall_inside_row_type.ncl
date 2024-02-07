# test.type = 'pass'
# eval = 'typecheck'

# regression test for https://github.com/tweag/nickel/issues/1027
let r : { id : forall a. a -> a } = { id = fun r => r } in
r.id "x"
