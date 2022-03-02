{
  run = fun n m =>
    let prefix = fun n =>
      array.generate (fun _n => "a") n
      |> array.foldl (fun x y => x ++ y) "" in
    let make_rec_step = fun state k =>
      let name = state.prev_name ++ (string.from_num k) in
      {
        value = state.value & {"%{name}" = {}},
        prev_name = name,
      } in
    let top_array = array.generate function.id m in
    let make_rec = fun m =>
      let l = array.generate function.id n in
      let result = array.foldl make_rec_step {value = {}, prev_name = prefix m} l in
      result.value in
    array.map make_rec top_array
    |> array.foldl (fun r1 r2 => r1 & r2) {}
}
