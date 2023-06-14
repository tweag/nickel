{
  contracts = {
    AllOf = fun contracts label value =>
      std.array.fold_right
        (
          fun c acc =>
            c
              label
              acc
        )
        contracts
        value,
    Dummy = fun label value => value,
    OneOf = fun values =>
      std.contract.from_predicate
        (
          fun value =>
            std.array.elem
              value
              values
        ),
    Nullable = fun ctr label value =>
      if value == null then
        null
      else
        std.contract.apply ctr label value,
    NotEq = fun x => std.contract.from_predicate (fun y => x != y),
    GreaterEq = fun x => std.contract.from_predicate (fun y => y >= x),
    Greater = fun x => std.contract.from_predicate (fun y => y > x),
    Smaller = fun x => std.contract.from_predicate (fun y => y < x),
    SmallerEq = fun x => std.contract.from_predicate (fun y => y <= x),
    MatchRegexp = fun regex label value =>
      let str_match = std.string.is_match regex in
      if std.is_string value then
        if str_match value then
          value
        else
          std.contract.blame_with_message "no match" label
      else
        std.contract.blame_with_message "not a string" label,
    PseudoOr = fun alts label value =>
      std.array.fold_right
        (
          fun ctr rest =>
            if ctr.pred value then
              ctr.contract value
            else
              rest
        )
        (std.contract.blame_with_message "no alternative matched" label),
    OrableFromPred : (Dyn -> Bool) -> { pred : Dyn -> Bool, contract : Dyn -> Dyn -> Dyn },
    OrableFromPred = fun pred_ =>
      {
        pred = pred_,
        contract = fun _lbl value => value,
      },
  },

  records = {
    mapToList = fun f r =>
      r
      |> std.record.map f
      |> std.record.values,

    toList = fun r =>
      r
      |> std.record.fields
      |> std.array.map (fun key_ => { key = key_, value = r."%{key}" }),
  }
}
