{
  functions = {
    id : forall a. a -> a
    | doc "The identity function"
    = fun x => x,

    const : forall a b. a -> b -> a
    | doc "The const function, results in the first argument."
    = fun x y => x,
  }
}
