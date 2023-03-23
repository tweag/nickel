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
        "%
      = fun label value =>
       if %typeof% value == `Array then
         if %length% value != 0 then
           value
         else
           %blame% (%label_with_message% "empty array" label)
       else
         %blame% (%label_with_message% "not a array" label),

    first : forall a. Array a -> a
      | doc m%"
        Results in the first element of the given array.

        For example:
        ```nickel
          first [ "this is the head", "this is not" ] =>
            "this is the head"
        ```
        "%
      = fun l => %head% l,

    last : forall a. Array a -> a
      | doc "TODO"
      = fun l => %elem_at% l (%length% l - 1),

    drop_first : forall a. Array a -> Array a
      | doc m%"
        Results in the given array without the first element.

        For example:
        ```nickel
          drop_first [ 1, 2, 3 ] =>
            [ 2, 3 ]
        ```
        "%
      = fun array => %array_slice% 1 (%length% array) array,

    drop_last : forall a. Array a -> Array a
      | doc m%"
        Results in the given array without the last element.

        For example:
        ```nickel
          drop_last [ 1, 2, 3 ] =>
            [ 1, 2 ]
        ```
        "%
      = fun array => %array_slice% 0 (%length% array - 1) array,

    length : forall a. Array a -> Number
      | doc m%"
        Results in a number representing the length of the given array.

        For example:
        ```nickel
          length [ "Hello,", " World!" ] =>
            2
        ```
        "%
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
        "%
      = fun f l => %map% l f,

    at : forall a. Number -> Array a -> a
      | doc m%"
        Retrieves the n'th element from a array (0-indexed).

        For example:
        ```nickel
          at 3 [ "zero" "one" "two" "three" "four" ] =>
            "three"
        ```
        "%
      = fun n l => %elem_at% l n,

    concat : forall a. Array a -> Array a -> Array a
      | doc m%"
        Concatenates two arrays such that the second array is appended to the first.

        For example:
        ```nickel
          concat [ 1, 2, 3 ] [ 4, 5, 6 ] =>
            [ 1, 2, 3, 4, 5, 6 ]
        ```
        "%
      = fun l1 l2 => l1 @ l2,

    fold_left : forall a b. (a -> b -> a) -> a -> Array b -> a
      | doc m%"
        Fold a function over an array. Folds serve a similar purpose to loops or
        iterators in a functional language like Nickel. `fold_left` iterates over
        an array, by repeatedly applying a function over each element, threading
        an additional arbitrary state (the accumulator, of type `a` in the
        signature).

        `fold_left f init [x1, x2, ..., xn]` results in `f (... (f (f init x1) x2) ...) xn`.

        This function is strict in the intermediate accumulator.

        # Left vs right

        Folds come in two variants, left and right. How to decide which one to
        use?

        - If the folded function isn't associative (such as subtraction), then
          each variant will give a different result. The choice is dictacted by
          which one you need. For example:

          ```nickel
          array.fold_right (-) 0 [1, 2, 3, 4]
           => -2
          array.fold_left (-) 0 [1, 2, 3, 4]
           => -10
          ```
        - If the folded function is associative, both `fold_right` and
          `fold_left` return the same result. In that case, **`fold_left` is
          generally preferred**, because it forces the evaluation of the
          intermediate results resulting in less memory consumption and overall
          better performance (outside of pathological cases).
          `fold_left` also iterates from the start of the array, which
          correponds to the usual behavior of loops and iterators in most
          programming languages. There is one case where `fold_right` might be
          preferred, see the next point.
        - If the folded function is associative but _(left) short-circuiting_,
          meaning that it can sometimes determine the result without using the
          right argument, then `fold_right` provides early return. An example is
          the boolean AND operator `&&`: when evaluating `left && right`, if
          `left` is `false`, the whole expression will evaluate to `false`
          without even evaluating `right`. Consider the following expression:

          ```nickel
          array.replicate 1000 true
          # gives [false, .. true 1000 times]
          |> array.prepend false
          |> array.fold_right (&&) [false]
          ```

          Here, `fold_right` will stop at the first element, and the operation
          runs in constant time, given the definition of `fold_right` and the
          lazy evaluation of Nickel. If we had used `fold_left` instead, which
          is closer to a standard iterator, we would have iterated over all of
          the 1000 elements of the array.

        # Examples

        ```nickel
          fold_left (fun acc e => acc + e) 0 [ 1, 2, 3 ] =>
            (((0 + 1) + 2) 3) =>
            6
        ```
        "%
    = fun f acc l =>
        let length = %length% l in
        if length == 0 then acc
        else
          let rec go = fun acc n =>
            if n == length then acc
            else
              let next_acc = %elem_at% l n |> f acc in
              %seq% next_acc (go next_acc (n+1))
          in
          go acc 0,

    fold_right : forall a b. (a -> b -> b) -> b -> Array a -> b
      | doc m%"
        Fold a function over an array. Folds serve a similar purpose to loops or
        iterators in a functional language like Nickel. `fold_right` iterates
        over an array, by repeatedly applying a function to each element and
        threading an additional arbitrary state (the accumulator of type `a` in
        the signature).

        `fold_right f init [x1, x2, ..., xn]` results in `f x1 (f x2 (... (f xn init) ...))`.

        # Left vs right

        Folds come in two variants, left and right. How to decide which one to
        use? Please refer to the documentation of `fold_left`.

        # Examples

        ```nickel
          fold_right (fun e acc => acc @ [e]) [] [ 1, 2, 3 ] =>
            ((([] @ [3]) @ [2]) @ [1]) =>
            [ 3, 2, 1 ]
        ```
        "%
      = fun f fst l =>
        let length = %length% l in
        let rec go = fun n =>
          if n == length then
            fst
          else
            go (n+1)
            |> f (%elem_at% l n)
        in go 0,

    prepend : forall a. a -> Array a -> Array a
      | doc m%"
        Construct an array given the first element and the rest of the array.

        For example:
        ```nickel
          prepend 1 [ 2, 3 ] =>
            [ 1, 2, 3 ]
        ```
        "%
      = fun x l => [x] @ l,

    append : forall a. a -> Array a -> Array a
      | doc m%"
        Construct an array given the last element and the rest of the array.

        For example:
        ```nickel
          append 3 [ 1, 2 ] =>
            [ 1, 2, 3 ]
        ```
        "%
      = fun x l => l @ [x],


    reverse : forall a. Array a -> Array a
      | doc m%"
        Reverses the order of a array.

        For example:
        ```nickel
          reverse [ 1, 2, 3 ] =>
            [ 3, 2, 1 ]
        ```
        "%
      = fun l => fold_left (fun acc e => [e] @ acc) [] l,

    filter : forall a. (a -> Bool) -> Array a -> Array a
      | doc m%"
        `filter f xs` keeps all elements from `xs` given that satisfy `f`.

        For example:
        ```nickel
          filter (fun x => x <= 3) [ 4, 3, 2, 5, 1 ] =>
            [ 3, 2, 1 ]
        ```
        "%
      = fun pred l => fold_left (fun acc x => if pred x then acc @ [x] else acc) [] l,

    flatten : forall a. Array (Array a) -> Array a
      | doc m%"
        Flatten a array of arrays to a single array, essentially concatenating all arrays in the original array.

        For example:
        ```nickel
          flatten [[1, 2], [3, 4]] =>
            [1, 2, 3, 4]
        ```
        "%
      = fun l => fold_right (fun l acc => l @ acc) [] l,

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
        "%
      = fun pred l => fold_right (fun x acc => if pred x then acc else false) true l,

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
        "%
      = fun pred l => fold_right (fun x acc => if pred x then true else acc) false l,

    elem : forall a. a -> Array a -> Bool
      | doc m%"
        Results in true if the given element is a member of the array, false otherwise.

        For example:
        ```nickel
          elem 3 [ 1, 2, 3, 4, 5 ] =>
            true
        ```
        "%
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
        "%
      = fun pred l =>
        let aux = fun acc x =>
          if (pred x) then
            {right = acc.right @ [x], wrong = acc.wrong}
          else
            {right = acc.right, wrong = acc.wrong @ [x]}
        in
        fold_left aux {right = [], wrong = []} l,

    generate : forall a. (Number -> a) -> Number -> Array a
      | doc m%"
        `generate f n` produces a array of length `n` by applying `f` on increasing numbers:
         `[ f 0, f 1, ..., f (n - 1) ]`.

        For example:
        ```nickel
          generate function.id 4 =>
            [ 0, 1, 2, 3 ]
        ```
        "%
      = fun f n => %generate% n f,

    sort | forall a. (a -> a -> [| `Lesser, `Equal, `Greater |]) -> Array a -> Array a
      | doc m%"
        Sorts the given arrays based on the provided comparison operator.

        For example:
        ```nickel
          sort (fun x y => if x < y then `Lesser else if (x == y) then `Equal else `Greater) [ 4, 5, 1, 2 ] =>
            [ 1, 2, 4, 5 ]
        ```
        "%
      #TODO: maybe inline partition to avoid contract checks?
      = fun cmp l =>
        let first = %head% l in
        let parts = partition (fun x => (cmp x first == `Lesser)) (%tail% l) in
        if %length% l <= 1 then
          l
        else
          (sort cmp (parts.right)) @ [first] @ (sort cmp (parts.wrong)),

    flat_map : forall a b. (a -> Array b) -> Array a -> Array b
      | doc m%"
        First maps the given function over the array and then flattens the
        result.

        For example:
        ```nickel
        flat_map (fun x => [x, x]) [1, 2, 3]
          => [1, 1, 2, 2, 3, 3]
        ```
      "%
      = fun f xs => map f xs |> flatten,

    intersperse : forall a. a -> Array a -> Array a
      | doc m%"
        Intersperse the passed value between the elements of an array.

        For example:
        ```nickel
        intersperse ", " [ "Hello", "wonderful", "world!" ]
          => [ "Hello", ", ", "wonderful", ", ", "world!" ]
        intersperse ", " [ "Hello" ]
          => [ "Hello" ]
        intersperse ", " []
          => []
        ```
      "%
      = fun v arr =>
        if %length% arr <= 1
        then arr
        else [ %head% arr ] @ (%tail% arr |> flat_map (fun a => [v, a])),

    slice
      : forall a. Number -> Number -> Array a -> Array a
      | doc m%"
          `slice start end array` returns the slice of `array` between `start` (included) and
          `end` (excluded).

          # Preconditions

          In `slice start end value`, `start` and `end` must be positive
          integers such that `0 <= start <= end <= array.length value`.

          # Examples

          ```nickel
          slice 1 3 [ 0, 1, 2, 3, 4, 5]
            => [ 1, 2 ]
          slice 0 3 [ "Hello", "world", "!" ]
            => [ "Hello", "world", "!" ]
          slice 2 3 [ "Hello", "world", "!" ]
            => [ "!" ]
           ```
        "%
      = fun start end value => %array_slice% start end value,

    split_at
      : forall a. Number -> Array a -> {left: Array a, right: Array a}
      | doc m%"
          Splits an array in two at a given index, and puts all the elements
          to the left of the element at the given index (excluded) in the
          `left` field, and the rest of the array in the `right` field.

          # Preconditions

          In `split_at inded value`, `index` must be a positive integer such
          that `0 <= index <= array.length value`.

          # Examples

          ```nickel
          split_at 2 [ 0, 1, 2, 3, 4, 5]
            => { left = [ 2, 3, 4, 5 ], right = [ 0, 1 ] }
          split_at 0 [ "Hello", "world", "!" ]
            => { left = [  ], right = [ "Hello", "world", "!" ] }
          split_at 3 [ "Hello", "world", "!" ]
            => { left = [ "Hello", "world", "!" ], right = [  ] }
          ```
        "%
      = fun index value => {
        left = %array_slice% 0 index value,
        right = %array_slice% index (%length% value) value
      },

    replicate
      : forall a. Number -> a -> Array a
      | doc m%"
          `replicate n x` creates an array containing `x` exactly `n` times.

          # Preconditions

          `n` must be an integer greater or equal to `0`.

          # Examples

          ```nickel
          replicate 0 false
            => [ ]
          replicate 5 "x"
            => [ "x", "x", "x", "x", "x" ]
          ```
        "%
      = fun n x => %generate% n (fun _i => x),

    range_step
      : Number -> Number -> Number -> Array Number
      | doc m%"
          `range_step start end step` generates the array of numbers
          `[start, start + step, start + 2*step, ..]` up to the first element
          (excluded) larger than or equal to `end`

          # Preconditions

          In `range_step start end step`, `start` and `end` must satisfy `start
          <= end`. `step` must be strictly greater than `0`.

          # Examples

          ```nickel
          range_step (-1.5) 2 0.5
           => [ -1.5, -1, -0.5, 0, 0.5, 1, 1.5 ]
          ```
        "%
      = fun start end step => %generate%
          ((end - start) / step |> number.floor)
          (fun i => start + i*step),

    range
      : Number -> Number -> Array Number
      | doc m%"
          `range start end` generates the array of numbers
          `[start, start + 1, start + 2, ..]` up to the first element
          (excluded) larger than or equal to `end`.

          `range start end` is equivalent to `range_step start end 1`.

          # Preconditions

          In `range_step start end`, `start` and `end` must satisfy
          `start <= end`.

          # Examples

          ```nickel
          range 0 5
           => [ 0, 1, 2, 3, 4 ]
          ```
        "%
      = fun start end => range_step start end 1,

    reduce_left
      : forall a. (a -> a -> a) -> Array a -> a
      | doc m%"
        Reduces the elements to a single one, by repeatedly applying a reducing
        operation. If the array is empty, returns an empty array.

        `reduce_left` associates to the left, that is
        `reduce_left op [x1, x2, ..., xn]` results in `op (... (op (op x1 x2) x3) ...) xn`.

        `reduce_left` is the same as `fold_left`, but uses the first element as
        the initial accumulator.

        # Left vs right

        The rationale to decide between `fold_left` and `fold_right` applies to
        `reduce_left` and `reduce_right` as well. See the documentation of
        `fold_left`.

        # Examples

        ```nickel
          reduce_left (@) [ [1, 2], [3], [4,5] ]
            => (([1, 2] @ [3]) @ [4,5])
            => [ 1, 2, 4, 5 ]
          reduce_left (-) [ 1, 2, 3, 4]
            => ((1 - 2) - 3) - 4
            => -8
        ```
        "%
      = fun f array =>
        let first = %elem_at% array 0 in
        let rest = %array_slice% 1 (%length% array) array in
        fold_left f first rest,

    reduce_right
      : forall a. (a -> a -> a) -> Array a -> a
      | doc m%"
        Reduces the elements to a single one, by repeatedly applying a reducing
        operation. If the array is empty, returns an empty array.

        `reduce_right` associates to the right, that is
        `reduce_right op [x1, x2, ..., xn]` results in
        `op x1 (op x2 (... (op xn-1 xn) ...))`.

        `reduce_right` is the same as `fold_right`, but uses the last element as
        the initial element.

        # Left vs right

        The rationale to decide between `fold_left` and `fold_right` applies to
        `reduce_left` and `reduce_right` as well. See the documentation of
        `fold_left`.

        # Examples

        ```nickel
          reduce_right (@) [ [1, 2], [3], [4,5] ]
            => [1, 2] @ ([3] @ [4,5])
            => [ 1, 2, 4, 5 ]
          reduce_right (-) [ 1, 2, 3, 4]
            => 1 - (2 - (3 - 4))
            => -2
        ```
        "%
      = fun f array =>
         let last_index = %length% array - 1 in
         let last =  %elem_at% array last_index in
         let rest = %array_slice% 0 last_index array in
         fold_right f last rest,
  }
}
