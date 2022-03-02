let letter | string.CharLiteral -> string.CharLiteral = fun n => string.charFromCode (string.charCode "a" + (n % 26)) in

{
  right = {
    string = {
      run = fun n =>
        list.fold (fun x y => x ++ "a") (list.generate n (fun n => letter n))
    },
    nums = {
      run = fun n =>
        list.fold (fun x y => x*y + (x - y)) (list.generate n (fun n => n/2))
    },
    list_ = {
      run = fun n =>
        list.fold (fun x acc => [x] ++ acc) (list.generate n (fun n => [n]))
    },
  },
  left = {
    string = {
      run = fun n =>
        list.foldl (fun x y => x ++ "a") (list.generate n (fun n => letter n))
    },
    nums = {
      run = fun n =>
        list.foldl (fun x y => x*y + (x - y)) (list.generate n (fun n => n/2))
    },
    list_ = {
      run = fun n =>
        list.foldl (fun x acc => [x] ++ acc) (list.generate n (fun n => [n]))
    },
  }
}
