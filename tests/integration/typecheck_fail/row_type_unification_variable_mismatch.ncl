{
  split : forall a. Array { key: Str, value: a } -> { keys: Array Str, values: Array a } = fun pairs =>
    array.fold_right (fun pair acc =>
      {
        # Error: `pair.key` should be wrapped in an array before we concat.
        keys = acc.keys @ pair.key,
        values = acc.values @ [pair.value],
      })
      { keys = [], values = [] }
      pairs,
}
