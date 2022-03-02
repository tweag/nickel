// Example of simple custom contract, parametrized by a first argument.
// In practice, for this kind of simple predicate, one should rather use
// `contract.from_predicate`
let EqualsTo = fun reference_value label value =>
  if reference_value == value then
    value
  else
    contract.blame label in

let AlwaysTrue = EqualsTo true in
let AlwaysFalse = EqualsTo false in

// This contract says: `not` requires its argument to be true, and in return
// promise that the return value is false.
// Try passing `false` to `not`, or to use the identity function (replacing `!x`
// by `x`) to see contract errors appear.
let not | AlwaysTrue -> AlwaysFalse = fun x => !x in
not true
