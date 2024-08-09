# test.type = 'pass'

# Regression test for issue #2014 "Record row polymorphism can be destructive"
# (https://github.com/tweag/nickel/issues/2014)
let f : forall tail. { a : Number; tail } -> { a : Number; tail }
  = fun o => o
  in
f { a = 1, b = ".." } == { a = 1, b = ".." }
