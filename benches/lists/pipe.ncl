{
  run = fun n =>
    let l = list.generate (fun n => n*n) n in
    let l = list.filter (fun n => n % 2 == 0) l in
    let l = list.map (fun n => [n, n+1]) l in
    let l = list.flatten l in
    list.partition (fun n => n % 2 == 0) l
}
