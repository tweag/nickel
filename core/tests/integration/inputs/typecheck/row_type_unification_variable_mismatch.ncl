# test.type = 'error'
# repeat = 5
#
# [test.metadata]
# error = 'TypecheckError::RecordRowMismatch'

# Regression test following [#841](https://github.com/tweag/nickel/issues/841).
# Checks that type mismatches occurring in the tail of a row type with
# unification variables don't cause a panic.
{
  split : forall a. Array { key: String, value: a } -> { keys: Array String, values: Array a } = fun pairs =>
    std.array.fold_right (fun pair acc =>
      {
        # Error: `pair.key` should be wrapped in an array before we concat.
        keys = acc.keys @ pair.key,
        values = acc.values @ [pair.value],
      })
      { keys = [], values = [] }
      pairs,
}
