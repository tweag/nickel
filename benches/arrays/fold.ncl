let letter | Number -> string.CharLiteral = fun n =>
  string.code "a" + (n % 26)
  |> string.from_code in

{
  right = {
    strings = {
      run = fun n =>
        array.fold_right (fun x acc => acc ++ x) "" (array.generate (fun n => letter n) n)
    },
    nums = {
      run = fun n =>
        array.fold_right (fun x acc => x*acc + (x - acc)) 0 (array.generate (fun n => n/2) n)
    },
    arrays = {
      run = fun n =>
        array.fold_right (fun x acc => acc @ [x]) [] (array.generate (fun n => [n]) n)
    },
  },
  left = {
    strings = {
      run = fun n =>
        array.fold_left (fun acc x => acc ++ x) "" (array.generate (fun n => letter n) n)
    },
    nums = {
      run = fun n =>
        array.fold_left (fun acc x => x*acc + (x - acc)) 0 (array.generate (fun n => n/2) n)
    },
    arrays = {
      run = fun n =>
        array.fold_left (fun acc x => acc @ [x]) [] (array.generate (fun n => [n]) n)
    },
  }
}
