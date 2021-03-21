{
  strings = {
    run = fun n =>
      lists.fold (fun x y => x ++ "a") (lists.generate n (fun n => strings.charCode
      "a" + (n % 26)))
  },

  nums = {
    run = fun n =>
      lists.fold (fun x y => x*y + (x - y)) (lists.generate n (fun n => n/2))
  }

  lists = {
    run = fun n =>
      lists.fold (fun x acc => [x] ++ acc) (lists.generate n (fun n => [n]))
  }
}
