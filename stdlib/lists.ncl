{
  lists = {
    head : forall a. List a -> a = fun l => %head% l;

    tail : forall a. List a -> List a = fun l => %tail% l;

    length : forall a. List a -> Num = fun l => %length% l;

    map : forall a b. (a -> b) -> List a -> List b = fun f l => %map% l f;

    elemAt : forall a. List a -> Num -> a = fun l n => %elemAt% l n;

    concat : forall a. List a -> List a -> List a = fun l1 l2 => l1 @ l2;

    foldl : forall a b. (a -> b -> a) -> a -> List b -> a =
      fun f fst l =>
        if %length% l == 0 then
          fst
        else
          let rest = foldl f fst (%tail% l) in
          %seq% rest (f rest (%head% l));

    fold : forall a b. (a -> b -> b) -> List a -> b -> b =
      fun f l fst =>
        if %length% l == 0 then
          fst
        else
          f (%head% l) (fold f (%tail% l) fst);

    cons : forall a. a -> List a -> List a = fun x l => [x] @ l;

    filter : forall a. (a -> Bool) -> List a -> List a =
      fun pred l =>
        fold (fun x acc => if pred x then acc @ [x] else acc) l [];

    //TODO: this enforces a List contract at each iteration. Get rid of it once
    //parametrized list types lands and `fold` get a more precise type.
    flatten : forall a. List (List a) -> List a =
      fun l =>
        fold (fun l acc => l @ acc) l [];

    all : forall a. (a -> Bool) -> List a -> Bool =
      fun pred l =>
        fold (fun x acc => if pred x then acc else false) l true;

    any : forall a. (a -> Bool) -> List a -> Bool =
      fun pred l =>
        fold (fun x acc => if pred x then true else acc) l false;
  }
}
