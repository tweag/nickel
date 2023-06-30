{
  run = fun n =>
    std.array.generate (fun n => n * n) n
    |> std.array.filter (fun n => n % 2 == 0)
    |> std.array.map (fun n => [n, n + 1])
    |> std.array.flatten
    |> std.array.partition (fun n => n % 2 == 0)
}
