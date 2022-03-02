{
  sum = {
    run = fun n =>
      let numbers = array.generate function.id n in
      array.foldl (fun acc x => acc + x) 0 numbers
  },

  product = {
    run = fun n =>
      let numbers = array.generate function.id n in
      array.foldl (fun acc x => acc * x) 1 numbers
  },
}
