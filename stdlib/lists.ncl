{
  lists = {
    concat = Promise(List -> List -> List, fun l1 l2 => l1 @ l2);

    foldl = Promise(forall a. (a -> Dyn -> a) -> a -> List -> a,
      fun f fst l =>
        if length l == 0 then
          fst
        else
          let rest = foldl f fst (tail l) in
          seq rest (f rest (head l)));

    fold = Promise(forall a. (Dyn -> a -> a) -> List -> a -> a,
      fun f l fst =>
        if length l == 0 then
          fst
        else
          f (head l) (fold f (tail l) fst));

    cons = Promise(Dyn -> List -> List, fun x l => [x] @ l);

    filter = Promise((Dyn -> Bool) -> List -> List,
      fun pred l =>
        fold (fun x acc => if pred x then acc @ [x] else acc) l []);

    flatten = Promise(List -> List, fun l =>
      fold (fun l acc => acc @ Assume(List, l)) l []);

    all = Promise((Dyn -> Bool) -> List -> Bool,
      fun pred l =>
        fold (fun x acc => if pred x then acc else false) l true);

    any = Promise((Dyn -> Bool) -> List -> Bool,
      fun pred l =>
        fold (fun x acc => if pred x then true else acc) l false);
  }
}
