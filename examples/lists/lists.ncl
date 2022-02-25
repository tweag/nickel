// Example list functions. This code is illustrative: prefer using the list
// stdlib functions `list.map` and `list.fold` instead.
let my_list_lib = {
  map : forall a b. (a -> b) -> List a -> List b = fun f l =>
    if l == [] then
      []
    else
      let head = list.head l in
      let tail = list.tail l in
      [f head] @ map f tail,

  fold : forall a b. (a -> b -> b) -> List a -> b -> b =
      fun f l first =>
        if l == [] then
          first
        else
          let head = list.head l in
          let tail = list.tail l in
          f head (fold f tail first),
} in
// Compute `7!`
let l = my_list_lib.map (fun x => x+1) [1, 2, 3, 4, 5, 6] in
my_list_lib.fold (fun x acc => x * acc) l 1
