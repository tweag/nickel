{
  contracts = {
    AllOf = fun contracts label value => array.fold_right (fun c acc => c label
      acc) contracts value,
    Dummy = fun label value => value,
    OneOf = fun values => contract.from_predicate (fun value => array.elem value
      values),
    Nullable = fun ctr label value =>
      if value == null then null
      else contract.apply ctr label value,
    NotEq = fun x => contract.from_predicate (fun y => x != y),
    GreaterEq = fun x => contract.from_predicate (fun y => y >= x),
    Greater = fun x => contract.from_predicate (fun y => y > x),
    Smaller = fun x => contract.from_predicate (fun y => y < x),
    SmallerEq = fun x => contract.from_predicate (fun y => y <= x),
    MatchRegexp = fun regex label value =>
      let str_match = string.is_match regex in
      if builtin.is_string value then
        if str_match value then
          value
        else
          contract.blame_with_message "no match" label
      else contract.blame_with_message "not a string" label,
    PseudoOr = fun alts label value =>
      array.fold_right (fun ctr rest =>
        if ctr.pred value then
          ctr.contract value
        else
          rest)
        (contract.blame_with_message "no alternative matched" label),
    OrableFromPred : (Dyn -> Bool)
                     -> {pred : Dyn -> Bool, contract: Dyn -> Dyn -> Dyn },
    OrableFromPred = fun pred_ => {
      pred = pred_,
      contract = fun _lbl value => value,
    },
  },

  records = {
    mapToList = fun f r =>
      r
      |> record.map f
      |> record.values,

   toList = fun r =>
     r
     |> record.fields
     |> array.map (fun key_ => {key = key_, value = r."%{key}"}),
  }
}
