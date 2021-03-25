let letter | #(strings.CharLiteral) -> #(strings.CharLiteral)= fun n => strings.charFromCode (strings.charCode "a" + (n % 26)) in

{
  right = {
    strings = {
      run = fun n =>
        lists.fold (fun x y => x ++ "a") (lists.generate n (fun n => letter n))
    },
    nums = {
      run = fun n =>
        lists.fold (fun x y => x*y + (x - y)) (lists.generate n (fun n => n/2))
    },
    lists = {
      run = fun n =>
        lists.fold (fun x acc => [x] ++ acc) (lists.generate n (fun n => [n]))
    },
  },
  left = {
    right = {
      strings = {
        run = fun n =>
          lists.foldl (fun x y => x ++ "a") (lists.generate n (fun n => letter n))
      },
      nums = {
        run = fun n =>
          lists.foldl (fun x y => x*y + (x - y)) (lists.generate n (fun n => n/2))
      },
      lists = {
        run = fun n =>
          lists.foldl (fun x acc => [x] ++ acc) (lists.generate n (fun n => [n]))
      },
    },
}
