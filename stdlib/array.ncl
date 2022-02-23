{
  array = {
    NonEmpty
      | doc m%"
        Contract to ensure the given array is not empty.

        For example:
        ```nickel
          ([] | NonEmpty) =>
            error
          ([ 1 ] | NonEmpty) =>
            [ 1 ]
        ```
        "%m
      = fun label value =>
       if %is_array% value then
         if %length% value != 0 then
           value
         else
           %blame% (%tag% "empty array" label)
       else
         %blame% (%tag% "not a array" label),

    head : forall a. Array a -> a
      | doc m%"
        Results in the head (first element) of the given array.

        For example:
        ```nickel
          head [ "this is the head", "this is not" ] =>
            "this is the head"
        ```
        "%m
      = fun l => %head% l,

    tail : forall a. Array a -> Array a
      | doc m%"
        Results in the tail (everything but the first element) of the given array.

        For example:
        ```nickel
          tail [ 1, 2, 3 ] =>
            [ 2, 3 ]
        ```
        "%m
      = fun l => %tail% l,

    length : forall a. Array a -> Num
      | doc m%"
        Results in a number representing the length of the given array.

        For example:
        ```nickel
          length [ "Hello,", " World!" ] =>
            2
        ```
        "%m
      = fun l => %length% l,

    map : forall a b. (a -> b) -> Array a -> Array b
      | doc m%"
        `map f [x1, x2, ..., xn]` applies function `f` to every element in the array,
        resulting in `[f x1, f x2, ... f xn]`

        For example:
        ```nickel
          map (fun x => x + 1) [ 1, 2, 3 ] =>
            [ 2, 3, 4 ]
        ```
        "%m
      = fun f l => %map% l f,

    elem_at : forall a. Num -> Array a -> a
      | doc m%"
        Retrieves the n'th element from a array (0-indexed).

        For example:
        ```nickel
          elem_at 3 [ "zero" "one" "two" "three" "four" ] =>
            "three"
        ```
        "%m
      = fun n l => %elem_at% l n,

    concat : forall a. Array a -> Array a -> Array a
      | doc m%"
        Concatenates two arrays such that the second array is appended to the first.

        For example:
        ```nickel
          concat [ 1, 2, 3 ] [ 4, 5, 6 ] =>
            [ 1, 2, 3, 4, 5, 6 ]
        ```
        "%m
      = fun l1 l2 => l1 @ l2,

    foldl : forall a b. (a -> b -> a) -> a -> Array b -> a
      | doc m%"
        Fold a function over a array.
        `foldl f init [x1, x2, ..., xn]` results in `f (... (f (f init x1) x2) ...) xn`.

        This function is strict in the intermediate accumulator.

        For example:
        ```nickel
          foldl (fun acc e => acc + e) 0 [ 1, 2, 3 ] =>
            (((0 + 1) + 2) 3) =>
            6
        ```
        "%m
      = fun f acc l =>
        if %length% l == 0 then
          acc
        else
          let newAcc = f acc (%head% l) in
          %seq% newAcc (foldl f newAcc (%tail% l)),

    fold : forall a b. (a -> b -> b) -> b -> Array a -> b
      | doc m%"
        Fold a function over a array.
        `fold f init [x1, x2, ..., xn]` results in `f x1 (f x2 (... (f xn init) ...))`.

        For example:
        ```nickel
          fold (fun e acc => acc @ [e]) [] [ 1, 2, 3 ] =>
            ((([] @ [3]) @ [2]) @ [1]) =>
            [ 3, 2, 1 ]
        ```
        "%m
      = fun f fst l =>
        if %length% l == 0 then
          fst
        else
          f (%head% l) (fold f fst (%tail% l)),

    cons : forall a. a -> Array a -> Array a
      | doc m%"
        Construct a array given the head and tail of the array.

        For example:
        ```nickel
          cons 1 [ 2, 3 ] =>
            [ 1, 2, 3 ]
        ```
        "%m
      = fun x l => [x] @ l,

    reverse : forall a. Array a -> Array a
      | doc m%"
        Reverses the order of a array.

        For example:
        ```nickel
          reverse [ 1, 2, 3 ] =>
            [ 3, 2, 1 ]
        ```
        "%m
      = fun l => foldl (fun acc e => [e] @ acc) [] l,

    filter : forall a. (a -> Bool) -> Array a -> Array a
      | doc m%"
        `filter f xs` keeps all elements from `xs` given that satisfy `f`.

        For example:
        ```nickel
          filter (fun x => x <= 3) [ 4, 3, 2, 5, 1 ] =>
            [ 3, 2, 1 ]
        ```
        "%m
      = fun pred l => foldl (fun acc x => if pred x then acc @ [x] else acc) [] l,

    flatten : forall a. Array (Array a) -> Array a
      | doc m%"
        Flatten a array of arrays to a single array, essentially concatenating all arrays in the original array.

        For example:
        ```nickel
          flatten [[1, 2], [3, 4]] =>
            [1, 2, 3, 4]
        ```
        "%m
      = fun l => fold (fun l acc => l @ acc) [] l,

    all : forall a. (a -> Bool) -> Array a -> Bool
      | doc m%"
        Results in true if all elements in the given array satisfy the predicate, false otherwise.

        For example:
        ```nickel
          all (fun x => x < 3) [ 1, 2 ] =>
            true
          all (fun x => x < 3) [ 1, 2 3 ] =>
            false
        ```
        "%m
      = fun pred l => fold (fun x acc => if pred x then acc else false) true l,

    any : forall a. (a -> Bool) -> Array a -> Bool
      | doc m%"
        Results in false if no elements in the given array satisfy the predicate, true otherwise.

        For example:
        ```nickel
          any (fun x => x < 3) [ 1, 2, 3, 4 ] =>
            true
          any (fun x => x < 3) [ 5, 6, 7, 8 ] =>
            false
        ```
        "%m
      = fun pred l => fold (fun x acc => if pred x then true else acc) false l,

    elem : Dyn -> Array -> Bool
      | doc m%"
        Results in true if the given element is a member of the array, false otherwise.

        For example:
        ```nickel
          elem 3 [ 1, 2, 3, 4, 5 ] =>
            true
        ```
        "%m
      = fun elt => any (fun x => x == elt),

    partition : forall a. (a -> Bool) -> Array a -> {right: Array a, wrong: Array a}
      | doc m%"
        Partitions the given array in two new arrays: those containing the elements that satisfy the predicate, and those
        that do not.

        For example:
        ```nickel
          partition (fun x => x < 5) [ 2, 4, 5, 3, 7, 8, 6 ] =>
            { right = [ 3, 4, 2 ], wrong = [ 6, 8, 7, 5 ] }
        ```
        "%m
      = fun pred l =>
        let aux = fun acc x =>
          if (pred x) then
            {right = acc.right @ [x], wrong = acc.wrong}
          else
            {right = acc.right, wrong = acc.wrong @ [x]}
        in
        foldl aux {right = [], wrong = []} l,

    generate : forall a. (Num -> a) -> Num -> Array a
      | doc m%"
        `generate f n` produces a array of length `n` by applying `f` on increasing numbers:
         `[ f 0, f 1, ..., f (n - 1) ]`.

        For example:
        ```nickel
          generate functions.id 4 =>
            [ 0, 1, 2, 3 ]
        ```
        "%m
      = fun f n => %generate% n f,

    sort : forall a. (a -> a -> [| Less, Equal, Greater |]) -> Array a -> Array a
      | doc m%"
        Sorts the given arrays based on the provided comparison operator.

        For example:
        ```nickel
          sort (fun x y => if x < y then `Less else if (x == y) then `Equal else `Greater) [ 4, 5, 1, 2 ] =>
            [ 1, 2, 4, 5 ]
        ```
        "%m
      = fun cmp l =>
        let first = %head% l in
        let parts = partition (fun x => (cmp x first == `Less)) (%tail% l) in
        if %length% l <= 1 then
          l
        else
          (sort cmp (parts.right)) @ [first] @ (sort cmp (parts.wrong)),
  }
}
