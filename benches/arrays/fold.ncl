let letter | string.CharLiteral -> string.CharLiteral = fun n =>
  string.char_code "a" + (n % 26)
  |> string.char_from_code in

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
    arrays = {
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
    arrays = {
      run = fun n =>
        array.foldl (fun x acc => [x] ++ acc) (array.generate n (fun n => [n]))
    },
  }
}
