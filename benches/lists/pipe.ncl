{
  run = fun n =>
    let l = lists.generate (fun n => n*n) n in
    let l = lists.filter (fun n => n % 2 == 0) l in
    let l = lists.map (fun n => [n, n+1]) l in
    let l = lists.flatten l in
    lists.partition (fun n => n % 2 == 0) l
}
