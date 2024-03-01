# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::RecordRowConflict'
#
# [test.metadata.expectation]
# row = 'a'

# Regression test for issue #1393 (https://github.com/tweag/nickel/issues/1393).
# Ensure that row constraints are properly checked, indepedently of the order of
# row declarations.
let f
  | forall r. {; r } -> { a : Number, b : Number; r }
  # the implementation doesn't matter, we care about types
  = fun r => null
  in
let g = fun x => 'a in
g ((f { x = 0, y = 0, a = 0 }) : _)

