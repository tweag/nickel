{
  run = fun n m =>
    let prefix = fun n =>
      std.array.generate (fun _n => "a") n
      |> std.array.fold_left (fun x y => x ++ y) ""
    in
    let make_rec_step = fun state k =>
      let name = state.prev_name ++ (std.string.from_number k) in
      {
        value = state.value & { "%{name}" = {} },
        prev_name = name,
      }
    in
    let top_array = std.array.generate std.function.id m in
    let make_rec = fun m =>
      let l = std.array.generate std.function.id n in
      let result = std.array.fold_left make_rec_step { value = {}, prev_name = prefix m } l in
      result.value
    in
    std.array.map make_rec top_array
    |> std.array.fold_left (fun r1 r2 => r1 & r2) {}
}
