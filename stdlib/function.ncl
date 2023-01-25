{
  function = {
    id : forall a. a -> a
    | doc "The identity function"
    = fun x => x,

    const : forall a b. a -> b -> a
    | doc "The const function, results in the first argument."
    = fun x y => x,

    compose : forall a b c. (b -> c) -> (a -> b) -> (a -> c)
    | doc "TODO"
    = fun g f x => x |> f |> g,

    flip : forall a b c. (a -> b -> c) -> b -> a -> c
    | doc "TODO"
    = fun f x y => f y x,

    fst : forall a b. a -> b -> a
    | doc "TODO"
    = const,

    snd : forall a b. a -> b -> b
    | doc "TODO"
    = flip const,

    pipe | Dyn -> Array (Dyn -> Dyn) -> Dyn
    | doc "TODO"
    = fun x fs => array.foldl (|>) x fs,
  }
}
