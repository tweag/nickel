# capture = 'stderr'
# command = []

# Regression test for https://github.com/tweag/nickel/issues/1021
let Foo = { foo : Number } in %force% (((fun x => { foo = "a" }) | Dyn -> Foo) null)