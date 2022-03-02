{
  run = fun n m =>
    let prefix = fun n =>
      let l = list.generate (fun _n => "a") n in
      list.foldl (fun x y => x ++ y) "" l in
    let makeRecStep = fun state k =>
      let name = state.prevName ++ (string.from_num k) in
      {
        value = state.value & {"%{name}" = {}},
        prevName = name,
      } in
    let topList = list.generate function.id m in
    let makeRec = fun m =>
      let l = list.generate function.id n in
      let result = list.foldl makeRecStep {value = {}, prevName = prefix m} l in
      result.value in
    let l = list.map makeRec topList in
    list.foldl (fun r1 r2 => r1 & r2) {} l
}
