# capture = 'stderr'
# command = []

# Regression test for https://github.com/tweag/nickel/issues/1021
let Foo = Number -> Number in ((fun x => "a") | Foo) 0