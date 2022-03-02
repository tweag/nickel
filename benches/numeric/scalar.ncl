{
  map2 = fun f l1 l2 =>
    if l1 == [] || l2 == [] then
      []
    else
      [f (list.head l1) (list.head l2)]
      @ map2 f (list.tail l1) (list.tail l2),

  run = fun n =>
    let left = list.generate function.id n in
    let right = list.generate (fun n => n*n/2) n in
    let prod = map2 (fun x y => x * y) left right in
    list.foldl (fun x y => x + y) 0 prod
}
