{
  lists = {
    concat = fun l1 l2 => l1 @ l2;

    foldl = fun f fst l =>
      if isZero (length l) then
        fst
      else
        let rest = foldl f fst (tail l) in
        seq rest (f rest (head l));

    fold = fun f l fst =>
      if isZero (length l) then
        fst
      else
        f (head l) (fold f (tail l) fst);

    cons = fun x l => [x] @ l;

    filter = fun pred l =>
      fold (fun x acc => if pred x then acc @ [x] else acc) l [];

    flatten = fun l =>
      fold (fun l acc => acc @ l) l [];

    all = fun pred l =>
      fold (fun x acc => if pred x then acc else false) l true;

    any = fun pred l =>
      fold (fun x acc => if pred x then true else acc) l false;
  }
}
