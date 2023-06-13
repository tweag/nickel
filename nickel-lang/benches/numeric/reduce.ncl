{
  sum = {
    run = fun n =>
      let numbers = std.array.generate std.function.id n in
      std.array.fold_left (fun acc x => acc + x) 0 numbers
  },

  product = {
    run = fun n =>
      let numbers = std.array.generate std.function.id n in
      std.array.fold_left (fun acc x => acc * x) 1 numbers
  },
}
