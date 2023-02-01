{
  function = {
    id : forall a. a -> a
    | doc "The identity function"
    = fun x => x,

    const : forall a b. a -> b -> a
    | doc "The const function, results in the first argument."
    = fun x y => x,

    compose : forall a b c. (b -> c) -> (a -> b) -> (a -> c)
    | doc "Function composition, right to left."
    = fun g f x => x |> f |> g,

    flip : forall a b c. (a -> b -> c) -> b -> a -> c
    | doc "Flip the argument order for a curried function."
    = fun f x y => f y x,

    fst : forall a b. a -> b -> a
    | doc "Always returns the first argument."
    = const,

    snd : forall a b. a -> b -> b
    | doc "Always returns the second argument."
    = flip const,

    pipe | Dyn -> Array (Dyn -> Dyn) -> Dyn
    | doc m%%"
      Apply an array of functions to a value, in order.

      For example:
      ```nickel
      pipe 2 [ (+) 2, (+) 3 ]
        => 7
      pipe `World [ string.from, fun s => "Hello, %{s}!" ]
        => "Hello, World!"
      ```
    "%%
    = fun x fs => array.foldl (|>) x fs,
  }
}
