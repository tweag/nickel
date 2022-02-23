let letter | strings.CharLiteral -> strings.CharLiteral = fun n => strings.charFromCode (strings.charCode "a" + (n % 26)) in

{
  right = {
    strings = {
      run = fun n =>
        array.fold (fun x y => x ++ "a") (array.generate n (fun n => letter n))
    },
    nums = {
      run = fun n =>
        array.fold (fun x y => x*y + (x - y)) (array.generate n (fun n => n/2))
    },
    array_ = {
      run = fun n =>
        array.fold (fun x acc => [x] ++ acc) (array.generate n (fun n => [n]))
    },
  },
  left = {
    strings = {
      run = fun n =>
        array.foldl (fun x y => x ++ "a") (array.generate n (fun n => letter n))
    },
    nums = {
      run = fun n =>
        array.foldl (fun x y => x*y + (x - y)) (array.generate n (fun n => n/2))
    },
    array_ = {
      run = fun n =>
        array.foldl (fun x acc => [x] ++ acc) (array.generate n (fun n => [n]))
    },
  }
}
