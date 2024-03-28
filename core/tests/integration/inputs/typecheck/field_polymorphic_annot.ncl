# test.type = 'pass'

# Regression test for https://github.com/tweag/nickel/issues/1690
let lib : _ = { id : forall a. a -> a = std.function.id } in
lib.id true
