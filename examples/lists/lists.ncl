// Example list functions. This code is illustrative: prefer using the list
// stdlib functions `lists.map` and `lists.fold` instead.
let my_list_lib = {
  map : forall a b. (a -> b) -> List a -> List b = fun f list =>
    if list == [] then
      []
    else
      let head = lists.head list in
      let tail = lists.tail list in
      [f head] @ map f tail,

  fold : forall a b. (a -> b -> b) -> List a -> b -> b =
      fun f list first =>
        if list == [] then
          first
        else
          let head = lists.head list in
          let tail = lists.tail list in
          f head (fold f tail first),
} in
// Compute `7!`
let l = my_list_lib.map (fun x => x+1) [1, 2, 3, 4, 5, 6] in
my_list_lib.fold (fun x acc => x * acc) l 1
