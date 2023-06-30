{
  map2 = fun f l1 l2 =>
    if l1 == [] || l2 == [] then
      []
    else
      [f (std.array.first l1) (std.array.first l2)]
      @ map2 f (std.array.drop_first l1) (std.array.drop_first l2),

  run = fun n =>
    let left = std.array.generate std.function.id n in
    let right = std.array.generate (fun n => n * n / 2) n in
    let prod = map2 (fun x y => x * y) left right in
    std.array.fold_left (fun x y => x + y) 0 prod
}
