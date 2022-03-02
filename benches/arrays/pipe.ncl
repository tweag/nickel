{
  run = fun n =>
    array.generate (fun n => n*n) n
    |> array.filter (fun n => n % 2 == 0)
    |> array.map (fun n => [n, n+1])
    |> array.flatten
    |> array.partition (fun n => n % 2 == 0)
}
