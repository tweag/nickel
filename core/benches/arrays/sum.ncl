let rec sum | Array Number -> Number = match {
  [] => 0,
  [x, ..xs] => x + sum xs,
}
in
{
  run = fun n => std.array.generate (fun x => x + 1) n |> sum
}
