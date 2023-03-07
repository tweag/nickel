{
  map2 = fun f l1 l2 =>
    if l1 == [] || l2 == [] then
      []
    else
      [f (array.first l1) (array.first l2)]
      @ map2 f (array.drop_first l1) (array.drop_first l2),

  run = fun n =>
    let left = array.generate function.id n in
    let right = array.generate (fun n => n*n/2) n in
    let prod = map2 (fun x y => x * y) left right in
    array.fold_left (fun x y => x + y) 0 prod
}
