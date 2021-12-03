{
  sum = {
    run = fun n =>
      let numbers = lists.generate builtins.id n in
      lists.foldl (fun acc x => acc + x) 0 numbers
  },

  product = {
    run = fun n =>
      let numbers = lists.generate builtins.id n in
      lists.foldl (fun acc x => acc * x) 1 numbers
  },
}
