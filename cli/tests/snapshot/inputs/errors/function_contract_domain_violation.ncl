# capture = 'stderr'
# command = ['eval']

# Regression test for https://github.com/tweag/nickel/issues/1021
let Foo = Number -> Number in ((fun x => x) | Foo) "a"
