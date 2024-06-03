# test.type = 'pass'


[
  # accesses
  std.array.at 1 [1,2,3] == 2,
  std.array.at 1 (std.array.map (fun x => x + 1) [1,2,3]) == 3,

  # length
  std.array.length [] == 0,
  std.array.length [1,2,3] == 3,
  std.array.length ([] @ [1,2] @ [3,4] @ []) == 4,

  # sort
  let cmp = fun x y =>
    if x < y then 'Lesser
    else if x == y then 'Equal
    else 'Greater
  in std.array.sort cmp [3, 42, -1, -5] == [-5, -1, 3, 42],

  # Test case added after https://github.com/tweag/nickel/issues/154
  let x = 1 in let l = [x] @ [2] in (%array/at% l 0) == 1,

  let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
    let foldr_ =
      fun self => fun f acc array =>
        let length = %array/length% array in
        if length == 0 then
          acc
        else
          let head = %array/at% array 0 in
          let tail = %array/slice% 1 length array in
          let next_acc = self f acc tail in
          f next_acc head in
    let foldr = Y foldr_ in
    let and : Bool -> Bool -> Bool =
        fun x => fun y =>
            if x then
                if y then true else false
            else false
    in
    let all = fun pred array => foldr and true (%array/map% array pred) in
    let isZ = fun x => x == 0 in
    all isZ [0, 0, 0, 1] == false,

  std.array.zip_with (+) [1, 2, 3] [4, 5, 6] == [5, 7, 9],
  std.array.zip_with (*) [1, 2] [4, 5, 6] == [4, 10],
  std.array.zip_with (-) [1, 2, 3] [4, 5] == [-3, -3],

  std.array.map_with_index (+) [1, 2, 3] == [1, 3, 5],
]
|> std.test.assert_all
