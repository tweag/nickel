{
  run = fun n m =>
    let prefix = fun n =>
      let l = lists.generate (fun _n => "a") n in
      lists.foldl (fun x y => x ++ y) "" l in
    let makeRecStep = fun state k =>
      let name = state.prevName ++ (strings.from_num k) in
      {
        value = state.value & {"%{name}" = {}},
        prevName = name,
      } in
    let topList = lists.generate functions.id m in
    let makeRec = fun m =>
      let l = lists.generate functions.id n in
      let result = lists.foldl makeRecStep {value = {}, prevName = prefix m} l in
      result.value in
    let l = lists.map makeRec topList in
    lists.foldl (fun r1 r2 => r1 & r2) {} l
}
