// Example list functions. This code is illustrative: prefer using the list
// stdlib functions `lists.map` and `lists.fold` instead.
let my_list_lib = {
  // We have to wrap `head` and `tail` with the right annotations if we want our
  // map and fold to be statically typed. This is due to a current issue with
  // the how the stdlib is typed, see
  // https://github.com/tweag/nickel/issues/226. It won't be needed eventually.
  head | forall a. List a -> a = fun l => %head% l,
  tail | forall a. List a -> List a = fun l => %tail% l,

  map : forall a b. (a -> b) -> List a -> List b = fun f list =>
    if list == [] then
      []
    else
      let head = head list in
      let tail = tail list in
      [f head] @ map f tail,

  fold : forall a b. (a -> b -> b) -> List a -> b -> b =
      fun f list first =>
        if list == [] then
          first
        else
          f (head list) (fold f (tail list) first),
} in
// Compute `7!`
let l = my_list_lib.map (fun x => x+1) [1, 2, 3, 4, 5, 6] in
my_list_lib.fold (fun x acc => x * acc) l 1
