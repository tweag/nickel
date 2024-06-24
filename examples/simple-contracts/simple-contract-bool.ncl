# test = 'pass'

# Example of simple custom contract, parametrized by a first argument.
let EqualsTo = fun reference_value =>
  std.contract.from_predicate ((==) reference_value)
in

let AlwaysTrue = EqualsTo true in
let AlwaysFalse = EqualsTo false in

# This contract says: `not` requires its argument to be true, and in return
# promise that the return value is false.
# Try passing `false` to `not`, or to use the identity function (replacing `!x`
# by `x`) to see contract errors appear.
let not | AlwaysTrue -> AlwaysFalse = fun x => !x in
not true
