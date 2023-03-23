{
  function = {
    id
      : forall a. a -> a
      | doc m%"
        The identity function, that is
        ```nickel
        id x = x
        ```
        for any value `x`.
      "%
      = fun x => x,

    const
      : forall a b. a -> b -> a
      | doc m%"
        Always returns the first argument, ignoring the second.

        # Examples
        
        ```nickel
        const 5 42 == 5
        ```
      "%
      = fun x y => x,

    compose
      : forall a b c. (b -> c) -> (a -> b) -> (a -> c)
      | doc m%"
        Function composition, from right to left. That is,
        ```nickel
        compose f g x == f (g x)
        ```
      "%
      = fun g f x => x |> f |> g,

    flip
      : forall a b c. (a -> b -> c) -> b -> a -> c
      | doc m%%"
        Flips the argument order for a two-argument function.

        # Examples
        
        ```nickel
        flip (fun x y => "%{x} %{y}") "world!" "Hello,"
          => "Hello, world!"
        ```
      "%%
      = fun f x y => f y x,

    first
      : forall a b. a -> b -> a
      | doc m%"
        Always returns the first argument, ignoring the second.

        # Examples
        
        ```nickel
        first 5 7 => 5
        ```
      "%
      = const,

    second
      : forall a b. a -> b -> b
      | doc m%"
        Always returns the second argument, ignoring the first.

        # Examples
        
        ```nickel
        second 5 7 => 7
        ```
      "%
      = flip const,

    pipe
      : forall a. a -> Array (a -> a) -> a
      | doc m%%"
        Applies an array of functions to a value, in order.

        # Examples

        ```nickel
        pipe 2 [ (+) 2, (+) 3 ]
          => 7
        pipe `World [ string.from, fun s => "Hello, %{s}!" ]
          => "Hello, World!"
        ```
      "%%
      = fun x fs => array.fold_left (|>) x fs,
  }
}
