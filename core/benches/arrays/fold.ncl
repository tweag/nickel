let letter | Number -> std.string.Character
  = fun n =>
    let letters = std.string.characters "abcdefghijklmnopqrstuvwxyz" in
    std.array.at (n % 26) letters
  in

{
  right = {
    strings = {
      run = fun n =>
        std.array.fold_right (fun x acc => acc ++ x) "" (std.array.generate (fun n => letter n) n)
    },
    nums = {
      run = fun n =>
        std.array.fold_right (fun x acc => x * acc + (x - acc)) 0 (std.array.generate (fun n => n / 2) n)
    },
    arrays = {
      run = fun n =>
        std.array.fold_right (fun x acc => acc @ [x]) [] (std.array.generate (fun n => [n]) n)
    },
  },
  left = {
    strings = {
      run = fun n =>
        std.array.fold_left (fun acc x => acc ++ x) "" (std.array.generate (fun n => letter n) n)
    },
    nums = {
      run = fun n =>
        std.array.fold_left (fun acc x => x * acc + (x - acc)) 0 (std.array.generate (fun n => n / 2) n)
    },
    arrays = {
      run = fun n =>
        std.array.fold_left (fun acc x => acc @ [x]) [] (std.array.generate (fun n => [n]) n)
    },
  }
}
