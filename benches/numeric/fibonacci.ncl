{
  run =
    let rec f = fun n =>
      if n == 0 || n == 1 then
        1
      else
        f (n - 1) + f (n - 2)
    in f
}
