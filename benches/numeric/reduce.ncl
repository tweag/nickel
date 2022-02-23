{
  sum = {
    run = fun n =>
      let numbers = list.generate function.id n in
      list.foldl (fun acc x => acc + x) 0 numbers
  },

  product = {
    run = fun n =>
      let numbers = list.generate function.id n in
      list.foldl (fun acc x => acc * x) 1 numbers
  },
}
