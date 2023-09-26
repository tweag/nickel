# capture = 'stderr'
# command = ['eval']

# Regression test for https://github.com/tweag/nickel/issues/1021
let Foo = Array Number in %force% (["a"] | Foo)
