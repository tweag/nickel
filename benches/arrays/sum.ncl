let rec sum
  | Array Num -> Num 
  = fun xs =>
    if array.length xs == 0 then 0
    else array.first xs + sum (array.tail xs)
in {
  run = fun n => array.generate (fun x => x + 1) n |> sum
}
