let rec sum | Array Number -> Number
  = fun xs =>
    if std.array.length xs == 0 then
      0
    else
      std.array.first xs + sum (std.array.drop_first xs)
  in
{
  run = fun n => std.array.generate (fun x => x + 1) n |> sum
}
